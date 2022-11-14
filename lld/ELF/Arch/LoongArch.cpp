//===- LoongArch.cpp ------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "InputFiles.h"
#include "OutputSections.h"
#include "Symbols.h"
#include "SyntheticSections.h"
#include "Target.h"
#include "lld/Common/ErrorHandler.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/TimeProfiler.h"
#include <mutex>

using namespace llvm;
using namespace llvm::object;
using namespace llvm::support::endian;
using namespace llvm::ELF;
using namespace lld;
using namespace lld::elf;

namespace {

class LoongArch final : public TargetInfo {
public:
  LoongArch();
  uint32_t calcEFlags() const override;
  int64_t getImplicitAddend(const uint8_t *buf, RelType type) const override;
  void writeGotHeader(uint8_t *buf) const override;
  void writeGotPlt(uint8_t *buf, const Symbol &s) const override;
  void writeIgotPlt(uint8_t *buf, const Symbol &s) const override;
  void writePltHeader(uint8_t *buf) const override;
  void writePlt(uint8_t *buf, const Symbol &sym,
                uint64_t pltEntryAddr) const override;
  RelType getDynRel(RelType type) const override;
  RelExpr getRelExpr(RelType type, const Symbol &s,
                     const uint8_t *loc) const override;
  void relocate(uint8_t *loc, const Relocation &rel,
                uint64_t val) const override;

private:
  std::mutex mu;
};

} // end anonymous namespace

const uint64_t dtpOffset = 0x800;

enum Op {
  SUB_W = 0x00110000,
  SUB_D = 0x00118000,
  SRLI_W = 0x00448000,
  SRLI_D = 0x00450000,
  ADDI_W = 0x02800000,
  ADDI_D = 0x02c00000,
  ANDI = 0x03400000,
  PCADDU12I = 0x1c000000,
  LD_W = 0x28800000,
  LD_D = 0x28c00000,
  JIRL = 0x4c000000,
};

enum Reg {
  R_RA = 1,
  R_TP = 2,
  R_T0 = 12,
  R_T1 = 13,
  R_T2 = 14,
  R_T3 = 15,
};

static uint32_t hi20(uint32_t val) { return (val + 0x800) >> 12; }
static uint32_t lo12(uint32_t val) { return val & 0xfff; }

static uint32_t insn(uint32_t op, uint32_t d, uint32_t j, uint32_t k) {
  return op | d | (j << 5) | (k << 10);
}

// Extract bits v[begin:end], where range is inclusive.
static uint32_t extractBits(uint64_t v, uint32_t begin, uint32_t end) {
  return begin == 63 ? v >> end : (v & ((1ULL << (begin + 1)) - 1)) >> end;
}

static uint32_t setD5k16(uint32_t insn, uint32_t imm) {
  uint32_t immLo = extractBits(imm, 15, 0);
  uint32_t immHi = extractBits(imm, 20, 16);
  return (insn & 0xfc0003e0) | (immLo << 10) | immHi;
}

static uint32_t setD10k16(uint32_t insn, uint32_t imm) {
  uint32_t immLo = extractBits(imm, 15, 0);
  uint32_t immHi = extractBits(imm, 25, 16);
  return (insn & 0xfc000000) | (immLo << 10) | immHi;
}

static uint32_t setJ20(uint32_t insn, uint32_t imm) {
  return (insn & 0xfe00001f) | (extractBits(imm, 19, 0) << 5);
}

static uint32_t setK12(uint32_t insn, uint32_t imm) {
  return (insn & 0xffc003ff) | (extractBits(imm, 11, 0) << 10);
}

static uint32_t setK16(uint32_t insn, uint32_t imm) {
  return (insn & 0xfc0003ff) | (extractBits(imm, 15, 0) << 10);
}

LoongArch::LoongArch() {
  defaultMaxPageSize = 65536;
  write32(trapInstr.data(), 0x002a0000); // break 0

  copyRel = R_LARCH_COPY;
  pltRel = R_LARCH_JUMP_SLOT;
  relativeRel = R_LARCH_RELATIVE;
  iRelativeRel = R_LARCH_IRELATIVE;

  // Currently LP64* ABIs only
  assert(config->is64 && "32-bit is unimplemented");
  symbolicRel = R_LARCH_64;
  tlsModuleIndexRel = R_LARCH_TLS_DTPMOD64;
  tlsOffsetRel = R_LARCH_TLS_DTPREL64;
  tlsGotRel = R_LARCH_TLS_TPREL64;

  gotRel = symbolicRel;

  // .got[0] = _DYNAMIC
  gotHeaderEntriesNum = 1;

  // .got.plt[0] = _dl_runtime_resolve, .got.plt[1] = link_map
  gotPltHeaderEntriesNum = 2;

  pltHeaderSize = 32;
  pltEntrySize = 16;
  ipltEntrySize = 16;
}

static uint32_t getEFlags(InputFile *f) {
  assert(config->is64 && "32-bit is unimplemented");
  return cast<ObjFile<ELF64LE>>(f)->getObj().getHeader().e_flags;
}

uint32_t LoongArch::calcEFlags() const {
  // If there are only binary input files (from -b binary), use a
  // value of 0 for the ELF header flags.
  if (ctx.objectFiles.empty())
    return 0;

  uint32_t target = getEFlags(ctx.objectFiles.front());

  for (InputFile *f : ctx.objectFiles) {
    if ((getEFlags(f) & EF_LOONGARCH_ABI_MODIFIER_MASK) !=
        (target & EF_LOONGARCH_ABI_MODIFIER_MASK))
      error(toString(f) +
            ": cannot link object files with different ABI");

    // We cannot process object ABI v0 files, unlike ld.bfd.
    if ((getEFlags(f) & EF_LOONGARCH_OBJABI_MASK) != EF_LOONGARCH_OBJABI_V1)
      error(toString(f) + ": unsupported object file ABI version");
  }

  return target;
}

int64_t LoongArch::getImplicitAddend(const uint8_t *buf, RelType type) const {
  switch (type) {
  default:
    internalLinkerError(getErrorLocation(buf),
                        "cannot read addend for relocation " + toString(type));
    return 0;
  case R_LARCH_32:
  case R_LARCH_TLS_DTPMOD32:
  case R_LARCH_TLS_DTPREL32:
  case R_LARCH_TLS_TPREL32:
    return SignExtend64<32>(read32le(buf));
  case R_LARCH_64:
  case R_LARCH_TLS_DTPMOD64:
  case R_LARCH_TLS_DTPREL64:
  case R_LARCH_TLS_TPREL64:
    return read64le(buf);
  case R_LARCH_RELATIVE:
  case R_LARCH_IRELATIVE:
    return config->is64 ? read64le(buf) : read32le(buf);
  case R_LARCH_NONE:
  case R_LARCH_JUMP_SLOT:
    // These relocations are defined as not having an implicit addend.
    return 0;
  }
}

void LoongArch::writeGotHeader(uint8_t *buf) const {
  if (config->is64)
    write64le(buf, mainPart->dynamic->getVA());
  else
    write32le(buf, mainPart->dynamic->getVA());
}

void LoongArch::writeGotPlt(uint8_t *buf, const Symbol &s) const {
  if (config->is64)
    write64le(buf, in.plt->getVA());
  else
    write32le(buf, in.plt->getVA());
}

void LoongArch::writeIgotPlt(uint8_t *buf, const Symbol &s) const {
  if (config->writeAddends) {
    if (config->is64)
      write64le(buf, s.getVA());
    else
      write32le(buf, s.getVA());
  }
}

void LoongArch::writePltHeader(uint8_t *buf) const {
  // 1: pcaddu12i $t2, %pcrel_hi20(.got.plt)
  //    sub.[wd]  $t1, $t1, $t3
  //    ld.[wd]   $t3, $t2, %pcrel_lo12(1b)  ; t3 = _dl_runtime_resolve
  //    addi.[wd] $t1, $t1, -pltHeaderSize-12; t1 = &.plt[i] - &.plt[0]
  //    addi.[wd] $t0, $t2, %pcrel_lo12(1b)
  //    srli.[wd] $t1, $t1, (is64?1:2)       ; t1 = &.got.plt[i] - &.got.plt[0]
  //    ld.[wd]   $t0, $t0, Wordsize(t0)     ; t0 = link_map
  //    jr        $t3
  uint32_t offset = in.gotPlt->getVA() - in.plt->getVA();
  uint32_t sub = config->is64 ? SUB_D : SUB_W;
  uint32_t ld = config->is64 ? LD_D : LD_W;
  uint32_t addi = config->is64 ? ADDI_D : ADDI_W;
  uint32_t srli = config->is64 ? SRLI_D : SRLI_W;
  write32le(buf + 0, insn(PCADDU12I, R_T2, hi20(offset), 0));
  write32le(buf + 4, insn(sub, R_T1, R_T1, R_T3));
  write32le(buf + 8, insn(ld, R_T3, R_T2, lo12(offset)));
  write32le(buf + 12, insn(addi, R_T1, R_T1, lo12(-target->pltHeaderSize - 12)));
  write32le(buf + 16, insn(addi, R_T0, R_T2, lo12(offset)));
  write32le(buf + 20, insn(srli, R_T1, R_T1, config->is64 ? 1 : 2));
  write32le(buf + 24, insn(ld, R_T0, R_T0, config->wordsize));
  write32le(buf + 28, insn(JIRL, 0, R_T3, 0));
}

void LoongArch::writePlt(uint8_t *buf, const Symbol &sym,
                     uint64_t pltEntryAddr) const {
  // 1: pcaddu12i $t3, %pcrel_hi20(f@.got.plt)
  //    ld.[wd]   $t3, $t3, %pcrel_lo12(1b)
  //    jirl      $t1, $t3, 0
  //    nop
  uint32_t offset = sym.getGotPltVA() - pltEntryAddr;
  write32le(buf + 0, insn(PCADDU12I, R_T3, hi20(offset), 0));
  write32le(buf + 4,
            insn(config->is64 ? LD_D : LD_W, R_T3, R_T3, lo12(offset)));
  write32le(buf + 8, insn(JIRL, R_T1, R_T3, 0));
  write32le(buf + 12, insn(ANDI, 0, 0, 0));
}

RelType LoongArch::getDynRel(RelType type) const {
  return type == target->symbolicRel ? type
                                     : static_cast<RelType>(R_LARCH_NONE);
}

RelExpr LoongArch::getRelExpr(const RelType type, const Symbol &s,
                              const uint8_t *loc) const {
  switch (type) {
  case R_LARCH_NONE:
  case R_LARCH_MARK_LA:
  case R_LARCH_MARK_PCREL:
    return R_NONE;
  case R_LARCH_32:
  case R_LARCH_64:
  case R_LARCH_ABS_HI20:
  case R_LARCH_ABS_LO12:
  case R_LARCH_ABS64_LO20:
  case R_LARCH_ABS64_HI12:
    return R_ABS;
  case R_LARCH_TLS_DTPREL32:
  case R_LARCH_TLS_DTPREL64:
    return R_DTPREL;
  case R_LARCH_TLS_TPREL32:
  case R_LARCH_TLS_TPREL64:
  case R_LARCH_TLS_LE64_HI12:
  case R_LARCH_TLS_LE64_LO20:
  case R_LARCH_TLS_LE_HI20:
  case R_LARCH_TLS_LE_LO12:
    return R_TPREL;
  case R_LARCH_ADD8:
  case R_LARCH_ADD16:
  case R_LARCH_ADD32:
  case R_LARCH_ADD64:
  case R_LARCH_SUB8:
  case R_LARCH_SUB16:
  case R_LARCH_SUB32:
  case R_LARCH_SUB64:
    return R_RISCV_ADD;
  case R_LARCH_32_PCREL:
    return R_PC;
  case R_LARCH_B16:
  case R_LARCH_B21:
  case R_LARCH_B26:
    return R_PLT_PC;
  case R_LARCH_GOT_PC_HI20:
  case R_LARCH_GOT_PC_LO12:
  case R_LARCH_GOT64_PC_LO20:
  case R_LARCH_GOT64_PC_HI12:
    return R_LOONGARCH_GOT_PAGE_PC;
  case R_LARCH_TLS_GD_PC_HI20:
    return R_LOONGARCH_TLSGD_PAGE_PC;
  case R_LARCH_TLS_IE_PC_HI20:
  case R_LARCH_TLS_IE_PC_LO12:
  case R_LARCH_TLS_IE64_PC_LO20:
  case R_LARCH_TLS_IE64_PC_HI12:
    config->hasTlsIe = true;
    return R_GOT_PC;
  case R_LARCH_PCALA64_HI12:
  case R_LARCH_PCALA64_LO20:
  case R_LARCH_PCALA_HI20:
  case R_LARCH_PCALA_LO12:
    return R_LOONGARCH_PAGE_PC;
  case R_LARCH_GOT64_HI12:
  case R_LARCH_GOT64_LO20:
  case R_LARCH_GOT_HI20:
  case R_LARCH_GOT_LO12:
  case R_LARCH_TLS_IE64_HI12:
  case R_LARCH_TLS_IE64_LO20:
  case R_LARCH_TLS_IE_HI20:
  case R_LARCH_TLS_IE_LO12:
    return R_GOT;
  case R_LARCH_TLS_LD_HI20:
    return R_TLSLD_GOT;
  case R_LARCH_TLS_LD_PC_HI20:
    return R_LOONGARCH_TLSLD_PAGE_PC;
  case R_LARCH_TLS_GD_HI20:
    return R_TLSGD_GOT;
/*
  case R_LARCH_COPY:
  case R_LARCH_IRELATIVE:
  case R_LARCH_JUMP_SLOT:
  case R_LARCH_RELATIVE:
 */

  case R_LARCH_SOP_PUSH_PCREL:
  case R_LARCH_SOP_PUSH_ABSOLUTE:
  case R_LARCH_SOP_PUSH_DUP:
  case R_LARCH_SOP_PUSH_GPREL:
  case R_LARCH_SOP_PUSH_TLS_TPREL:
  case R_LARCH_SOP_PUSH_TLS_GOT:
  case R_LARCH_SOP_PUSH_TLS_GD:
  case R_LARCH_SOP_PUSH_PLT_PCREL:
  case R_LARCH_SOP_ASSERT:
  case R_LARCH_SOP_NOT:
  case R_LARCH_SOP_SUB:
  case R_LARCH_SOP_SL:
  case R_LARCH_SOP_SR:
  case R_LARCH_SOP_ADD:
  case R_LARCH_SOP_AND:
  case R_LARCH_SOP_IF_ELSE:
  case R_LARCH_SOP_POP_32_S_10_5:
  case R_LARCH_SOP_POP_32_U_10_12:
  case R_LARCH_SOP_POP_32_S_10_12:
  case R_LARCH_SOP_POP_32_S_10_16:
  case R_LARCH_SOP_POP_32_S_10_16_S2:
  case R_LARCH_SOP_POP_32_S_5_20:
  case R_LARCH_SOP_POP_32_S_0_5_10_16_S2:
  case R_LARCH_SOP_POP_32_S_0_10_10_16_S2:
  case R_LARCH_SOP_POP_32_U:
    error(getErrorLocation(loc) +
          "cannot handle stack-based LoongArch relocation " + toString(type) +
          " against symbol " + toString(s));
    return R_NONE;

  default:
    error(getErrorLocation(loc) + "unknown relocation " + Twine(type) +
          " against symbol " + toString(s));
    return R_NONE;
  }
}

void LoongArch::relocate(uint8_t *loc, const Relocation &rel,
                         uint64_t val) const {
  if (rel.type >= 64)
  #define DEBUG_TYPE "LLDLoongArch"
    LLVM_DEBUG(lld::message(
      "XXX " + getErrorLocation(loc) + ": " + toString(rel.type) +
      " off=0x" + Twine::utohexstr(rel.offset) +
      " val=0x" + Twine::utohexstr(val)));

  switch (rel.type) {
  case R_LARCH_32:
    write32le(loc, val);
    return;
  case R_LARCH_64:
    write64le(loc, val);
    return;

  case R_LARCH_B16: {
    checkInt(loc, val, 18, rel);
    checkAlignment(loc, val, 4, rel);
    write32le(loc, setK16(read32le(loc), val >> 2));
    return;
  }

  case R_LARCH_B21: {
    checkInt(loc, val, 23, rel);
    checkAlignment(loc, val, 4, rel);
    write32le(loc, setD5k16(read32le(loc), val >> 2));
    return;
  }

  case R_LARCH_B26: {
    checkInt(loc, val, 28, rel);
    checkAlignment(loc, val, 4, rel);
    write32le(loc, setD10k16(read32le(loc), val >> 2));
    return;
  }

  // Relocs writing val[11:0] to instruction slot Sk12.
  case R_LARCH_ABS_LO12: {
    // TODO: checkInt(loc, SignExtend64(hi, bits) >> 12, 20, rel);
    write32le(loc, setK12(read32le(loc), extractBits(val, 11, 0)));
    return;
  }
  case R_LARCH_PCALA_LO12:
  case R_LARCH_GOT_PC_LO12:
  case R_LARCH_GOT_LO12:
  case R_LARCH_TLS_LE_LO12:
  case R_LARCH_TLS_IE_PC_LO12:
  case R_LARCH_TLS_IE_LO12: {
    // TODO: checkInt(loc, SignExtend64(hi, bits) >> 12, 20, rel);
    write32le(loc, setK12(read32le(loc), extractBits(val, 11, 0)));
    return;
  }

  // Relocs writing val[31:12] (or the corresponding PC-region-aligned delta)
  // to instruction slot Sj20.
  case R_LARCH_ABS_HI20:
  case R_LARCH_PCALA_HI20:
  case R_LARCH_GOT_PC_HI20:
  case R_LARCH_GOT_HI20:
  case R_LARCH_TLS_LE_HI20:
  case R_LARCH_TLS_IE_PC_HI20:
  case R_LARCH_TLS_IE_HI20:
  case R_LARCH_TLS_LD_PC_HI20:
  case R_LARCH_TLS_LD_HI20:
  case R_LARCH_TLS_GD_PC_HI20:
  case R_LARCH_TLS_GD_HI20: {
    // TODO: checkInt(loc, SignExtend64(hi, bits) >> 12, 20, rel);
    write32le(loc, setJ20(read32le(loc), extractBits(val, 31, 12)));
    return;
  }

  // Relocs writing val[51:32] to instruction slot Sj20.
  case R_LARCH_ABS64_LO20:
  case R_LARCH_PCALA64_LO20:
  case R_LARCH_GOT64_PC_LO20:
  case R_LARCH_GOT64_LO20:
  case R_LARCH_TLS_LE64_LO20:
  case R_LARCH_TLS_IE64_PC_LO20:
  case R_LARCH_TLS_IE64_LO20: {
    // TODO: checkInt(loc, SignExtend64(hi, bits) >> 12, 20, rel);
    write32le(loc, setJ20(read32le(loc), extractBits(val, 51, 32)));
    return;
  }

  // Relocs writing val[63:52] to instruction slot Sk12.
  case R_LARCH_ABS64_HI12:
  case R_LARCH_PCALA64_HI12:
  case R_LARCH_GOT64_PC_HI12:
  case R_LARCH_GOT64_HI12:
  case R_LARCH_TLS_LE64_HI12:
  case R_LARCH_TLS_IE64_PC_HI12:
  case R_LARCH_TLS_IE64_HI12: {
    // TODO: checkInt(loc, SignExtend64(hi, bits) >> 12, 20, rel);
    write32le(loc, setK12(read32le(loc), extractBits(val, 63, 52)));
    return;
  }

  case R_LARCH_ADD8:
    *loc += val;
    return;
  case R_LARCH_ADD16:
    write16le(loc, read16le(loc) + val);
    return;
  case R_LARCH_ADD32:
    write32le(loc, read32le(loc) + val);
    return;
  case R_LARCH_ADD64:
    write64le(loc, read64le(loc) + val);
    return;
  case R_LARCH_SUB8:
    *loc -= val;
    return;
  case R_LARCH_SUB16:
    write16le(loc, read16le(loc) - val);
    return;
  case R_LARCH_SUB32:
    write32le(loc, read32le(loc) - val);
    return;
  case R_LARCH_SUB64:
    write64le(loc, read64le(loc) - val);
    return;
  case R_LARCH_32_PCREL:
    write32le(loc, val);
    return;

  case R_LARCH_TLS_DTPREL32:
    write32le(loc, val - dtpOffset);
    break;
  case R_LARCH_TLS_DTPREL64:
    write64le(loc, val - dtpOffset);
    break;

  case R_LARCH_MARK_LA:
  case R_LARCH_MARK_PCREL:
    // no-op
    return;

  case R_LARCH_RELAX:
    return; // Ignored (for now)

  // These relocs need a stack machine (read: global mutable state) to properly
  // compute, and are already deprecated after the release of LoongArch ELF
  // psABI v2.00, so we are not going to implement them.
  case R_LARCH_SOP_PUSH_PCREL:
  case R_LARCH_SOP_PUSH_ABSOLUTE:
  case R_LARCH_SOP_PUSH_DUP:
  case R_LARCH_SOP_PUSH_GPREL:
  case R_LARCH_SOP_PUSH_TLS_TPREL:
  case R_LARCH_SOP_PUSH_TLS_GOT:
  case R_LARCH_SOP_PUSH_TLS_GD:
  case R_LARCH_SOP_PUSH_PLT_PCREL:
  case R_LARCH_SOP_ASSERT:
  case R_LARCH_SOP_NOT:
  case R_LARCH_SOP_SUB:
  case R_LARCH_SOP_SL:
  case R_LARCH_SOP_SR:
  case R_LARCH_SOP_ADD:
  case R_LARCH_SOP_AND:
  case R_LARCH_SOP_IF_ELSE:
  case R_LARCH_SOP_POP_32_S_10_5:
  case R_LARCH_SOP_POP_32_U_10_12:
  case R_LARCH_SOP_POP_32_S_10_12:
  case R_LARCH_SOP_POP_32_S_10_16:
  case R_LARCH_SOP_POP_32_S_10_16_S2:
  case R_LARCH_SOP_POP_32_S_5_20:
  case R_LARCH_SOP_POP_32_S_0_5_10_16_S2:
  case R_LARCH_SOP_POP_32_S_0_10_10_16_S2:
  case R_LARCH_SOP_POP_32_U:
    error(getErrorLocation(loc) +
          "cannot handle stack-based LoongArch relocation (" + Twine(rel.type) +
          ")");
    return;

  default:
    llvm_unreachable("unknown relocation");
  }
}

TargetInfo *elf::getLoongArchTargetInfo() {
  static LoongArch target;
  return &target;
}
