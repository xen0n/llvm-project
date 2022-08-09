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

using namespace llvm;
using namespace llvm::object;
using namespace llvm::support::endian;
using namespace llvm::ELF;
using namespace lld;
using namespace lld::elf;

// The LoongArch `pcalau12i` is behaviorally equivalent to the AArch64 `adrp`,
// so a similar concept of "page" also applies here. A "page" is in fact just
// another way to refer to the 12-bit range allowed by the immediate field of
// the addi/ld/st instructions.
//
// Note: "pcalau12i" stands for something like "PC aligned add upper, from 12th
// bit, immediate". (The official ISA manual gives no such "full names".)
static uint64_t getLoongArchPage(uint64_t p) {
  return p & ~static_cast<uint64_t>(0xfff);
}

// Calculate the adjusted page offset between dest and PC.
//
// We must specially handle the cases when the low 12 bits of dest are seen
// as negative, because the instructions consuming it (ld, st, addi, etc.)
// all sign-extend the immediate, unlike AArch64. The higher bits need
// tweaking too, due to potential usage in patterns like:
//
//     pcalau12i A, %foo_hi20(sym)
//     addi.d    T, zero, %foo_lo12(sym)
//     lu32i.d   T, %foo64_lo20(sym)
//     lu52i.d   T, T, %foo64_hi12(sym)
//     ldx.d     A, A, T
//
// in which case the "pc + hi20" part is separately constructed from the rest
// which includes the higher 32-bit half and lo12, so the higher 32 bits need a
// conditional nudge too, due to the signed addition performed by the ldx/stx.
uint64_t elf::getLoongArchPageOffset(uint64_t dest, uint64_t pc) {
  uint64_t result = getLoongArchPage(dest) - getLoongArchPage(pc);
  if ((dest & 0xfff) > 0x7ff) {
    result += 0x1000;
    if (static_cast<int64_t>(result) > 0)
      result -= 0x100000000;
  }
  return result;
}

namespace {

class LoongArch final : public TargetInfo {
public:
  LoongArch();
  uint32_t calcEFlags() const override;
  int64_t getImplicitAddend(const uint8_t *buf, RelType type) const override;
  void writeGotPlt(uint8_t *buf, const Symbol &s) const override;
  void writeIgotPlt(uint8_t *buf, const Symbol &s) const override;
  void writePltHeader(uint8_t *buf) const override;
  void writePlt(uint8_t *buf, const Symbol &sym,
                uint64_t pltEntryAddr) const override;
  RelType getDynRel(RelType type) const override;
  RelExpr getRelExpr(RelType type, const Symbol &s,
                     const uint8_t *loc) const override;
  bool usesOnlyLowPageBits(RelType type) const override;
  void relocate(uint8_t *loc, const Relocation &rel,
                uint64_t val) const override;
};

} // end anonymous namespace

enum Op {
  SUB_W = 0x00110000,
  SUB_D = 0x00118000,
  BREAK = 0x002a0000,
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
  R_ZERO = 0,
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

static bool isJirl(uint32_t insn) {
  return (insn & 0xfc000000) == JIRL;
}

LoongArch::LoongArch() {
  // The LoongArch ISA itself does not have a limit on page sizes. According to
  // the ISA manual, the PS (page size) field in MTLB entries and CSR.STLBPS is
  // 6 bits wide, meaning the maximum page size is 2^63 which is equivalent to
  // "unlimited".
  // However, practically the maximum usable page size is constrained by the
  // kernel implementation, and 64KiB is the biggest non-huge page size
  // supported by Linux as of v6.3.
  defaultMaxPageSize = 65536;
  write32(trapInstr.data(), BREAK); // break 0

  copyRel = R_LARCH_COPY;
  pltRel = R_LARCH_JUMP_SLOT;
  relativeRel = R_LARCH_RELATIVE;
  iRelativeRel = R_LARCH_IRELATIVE;

  if (config->is64) {
    symbolicRel = R_LARCH_64;
    tlsModuleIndexRel = R_LARCH_TLS_DTPMOD64;
    tlsOffsetRel = R_LARCH_TLS_DTPREL64;
    tlsGotRel = R_LARCH_TLS_TPREL64;
  } else {
    symbolicRel = R_LARCH_32;
    tlsModuleIndexRel = R_LARCH_TLS_DTPMOD32;
    tlsOffsetRel = R_LARCH_TLS_DTPREL32;
    tlsGotRel = R_LARCH_TLS_TPREL32;
  }

  gotRel = symbolicRel;

  // .got.plt[0] = _dl_runtime_resolve, .got.plt[1] = link_map
  gotPltHeaderEntriesNum = 2;

  pltHeaderSize = 32;
  pltEntrySize = 16;
  ipltEntrySize = 16;
}

static uint32_t getEFlags(InputFile *f) {
  if (config->is64)
    return cast<ObjFile<ELF64LE>>(f)->getObj().getHeader().e_flags;
  return cast<ObjFile<ELF32LE>>(f)->getObj().getHeader().e_flags;
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

    // We cannot process psABI v1.x / object ABI v0 files (containing stack
    // relocations), unlike ld.bfd.
    //
    // Instead of blindly accepting every v0 object and only failing at
    // relocation processing time, just disallow interlink altogether. We
    // don't expect significant usage of object ABI v0 in the wild (the old
    // world may continue using object ABI v0 for a while, but as it's not
    // binary-compatible with the upstream i.e. new-world ecosystem, it's not
    // being considered here).
    //
    // There are briefly some new-world systems with object ABI v0 binaries too.
    // It is because these programs were built before v1 was finalized.
    // These are not supported either due to the extremely small number of them,
    // and the few impacted users are advised to simply rebuild world or
    // reinstall a recent system.
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
  // The LoongArch PLT is currently structured just like that of RISCV.
  // Annoyingly, this means the PLT is still using `pcaddu12i` to perform
  // PC-relative addressing (because `pcaddu12i` is the same as RISCV `auipc`),
  // in contrast to the AArch64-like page-offset scheme with `pcalau12i` that
  // is used everywhere else involving PC-relative operations in the LoongArch
  // ELF psABI v2.00.
  // The `pcrel_{hi20,lo12}` operators are illustrative only and not really
  // supported by LoongArch assemblers.
  //
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
  write32le(buf + 28, insn(JIRL, R_ZERO, R_T3, 0));
}

void LoongArch::writePlt(uint8_t *buf, const Symbol &sym,
                     uint64_t pltEntryAddr) const {
  // See the comment in writePltHeader for reason why pcaddu12i is used instead
  // of the pcalau12i that's more commonly seen in the ELF psABI v2.0 days.
  //
  // 1: pcaddu12i $t3, %pcrel_hi20(f@.got.plt)
  //    ld.[wd]   $t3, $t3, %pcrel_lo12(1b)
  //    jirl      $t1, $t3, 0
  //    nop
  uint32_t offset = sym.getGotPltVA() - pltEntryAddr;
  write32le(buf + 0, insn(PCADDU12I, R_T3, hi20(offset), 0));
  write32le(buf + 4,
            insn(config->is64 ? LD_D : LD_W, R_T3, R_T3, lo12(offset)));
  write32le(buf + 8, insn(JIRL, R_T1, R_T3, 0));
  write32le(buf + 12, insn(ANDI, R_ZERO, R_ZERO, 0));
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
  case R_LARCH_PCALA_LO12:
    // This may well be just R_ABS and nothing more, but unfortunately some
    // people had the brilliant idea of reusing the R_LARCH_PCALA_LO12 reloc on
    // JIRLs, for implementing the medium code model, seemingly for avoiding
    // having to add another reloc type just for that. The intended use case is
    // for making the libc_nonshared.a of glibc 2.37+ usable with large
    // applications like Firefox or Chromium, where an R_LARCH_B26 will overflow
    // otherwise. See [1] for details.
    //
    // This is bad for multiple reasons:
    //
    // * R_LARCH_PCALA_LO12 just places its computed value unmodified, but JIRL
    //   expects its immediate to be pre-shifted by 2 bits, making the reloc
    //   behavior dependent on the underlying input section content;
    // * R_LARCH_PCALA_LO12 leaves the upper 16-12+2=6 bits of JIRL's immediate
    //   operand unusable, making the usage forever a kludge: a proper reloc
    //   type has to be added *anyway*, to be able to leverage the PCADDU18I +
    //   JIRL combo, for example.
    //   (In this case an ISA revision (!) is arguably necessary, because there
    //   is not a PCALAU18I for stylistic consistency with the other psABI v2.00
    //   relocs. If PCADDU18I is to be used anyway, that would mean the new
    //   relocs are PCREL-style instead of PCALA; so for consistency, PCREL
    //   relocs for the other psABI v2.00 relocs should be added as well...)
    // * Inspection into the input section must be performed for every
    //   R_LARCH_PCALA_LO12 record, one of the most frequently used reloc types
    //   on LoongArch. But usage of the medium code model is not nearly as
    //   popular, so arguably ~100% of the checks are in fact wasted for very
    //   little gain.
    //
    // The problem is long known, and already reported with a suggested fix
    // back in September 2022 [2]. But no action has been taken and object
    // code with this usage has already appeared since glibc 2.37, so we have
    // to implement a similar workaround as the BFD linker does.
    //
    // [1]: https://sourceware.org/git/?p=glibc.git;a=commitdiff;h=9f482b73f41a9a1bbfb173aad0733d1c824c788a
    // [2]: https://github.com/loongson/LoongArch-Documentation/pull/69
    return isJirl(read32le(loc)) ? R_PLT : R_ABS;
  case R_LARCH_TLS_DTPREL32:
  case R_LARCH_TLS_DTPREL64:
    return R_DTPREL;
  case R_LARCH_TLS_TPREL32:
  case R_LARCH_TLS_TPREL64:
  case R_LARCH_TLS_LE_HI20:
  case R_LARCH_TLS_LE_LO12:
  case R_LARCH_TLS_LE64_LO20:
  case R_LARCH_TLS_LE64_HI12:
    return R_TPREL;
  case R_LARCH_ADD8:
  case R_LARCH_ADD16:
  case R_LARCH_ADD32:
  case R_LARCH_ADD64:
  case R_LARCH_SUB8:
  case R_LARCH_SUB16:
  case R_LARCH_SUB32:
  case R_LARCH_SUB64:
    // The LoongArch add/sub relocs behave like the RISCV counterparts; reuse
    // the RelExpr to avoid code duplication.
    return R_RISCV_ADD;
  case R_LARCH_32_PCREL:
    return R_PC;
  case R_LARCH_B16:
  case R_LARCH_B21:
  case R_LARCH_B26:
    return R_PLT_PC;
  case R_LARCH_GOT_PC_HI20:
  case R_LARCH_GOT64_PC_LO20:
  case R_LARCH_GOT64_PC_HI12:
  case R_LARCH_TLS_IE_PC_HI20:
  case R_LARCH_TLS_IE64_PC_LO20:
  case R_LARCH_TLS_IE64_PC_HI12:
    return R_LOONGARCH_GOT_PAGE_PC;
  case R_LARCH_GOT_PC_LO12:
  case R_LARCH_TLS_IE_PC_LO12:
    return R_LOONGARCH_GOT;
  case R_LARCH_TLS_LD_PC_HI20:
  case R_LARCH_TLS_GD_PC_HI20:
    return R_LOONGARCH_TLSGD_PAGE_PC;
  case R_LARCH_PCALA_HI20:
    // Why not R_LOONGARCH_PAGE_PC, majority of references don't go through PLT
    // anyway so why waste time checking only to get everything relaxed back to
    // it?
    //
    // This is again due to the R_LARCH_PCALA_LO12 on JIRL case, where we want
    // both the HI20 and LO12 to potentially refer to the PLT. But in reality
    // the HI20 reloc appears earlier, and the relocs don't contain enough
    // information to let us properly resolve semantics per symbol.
    // Unlike RISCV, our LO12 relocs *do not* point to their corresponding HI20
    // relocs, hence it is nearly impossible to 100% accurately determine each
    // HI20's "flavor" without taking big performance hits, in the presence of
    // edge cases (e.g. HI20 without pairing LO12, paired LO12 placed so far
    // apart that relationship is not certain anymore), and programmer mistakes
    // (e.g. as outlined in https://github.com/loongson/LoongArch-Documentation/pull/69).
    //
    // Ideally we would scan in an extra pass for all LO12s on JIRL, then mark
    // every HI20 reloc referring to the same symbol differently; this is not
    // feasible with the current function signature of getRelExpr that doesn't
    // allow for such inter-pass state.
    //
    // So, unfortunately we have to again workaround this quirk the same way as
    // BFD: assuming every R_LARCH_PCALA_HI20 is potentially PLT needing, only
    // relaxing to R_LOONGARCH_PAGE_PC if it's known not so at a later stage.
    return R_LOONGARCH_PLT_PAGE_PC;
  case R_LARCH_PCALA64_LO20:
  case R_LARCH_PCALA64_HI12:
    return R_LOONGARCH_PAGE_PC;
  case R_LARCH_GOT_HI20:
  case R_LARCH_GOT_LO12:
  case R_LARCH_GOT64_LO20:
  case R_LARCH_GOT64_HI12:
  case R_LARCH_TLS_IE_HI20:
  case R_LARCH_TLS_IE_LO12:
  case R_LARCH_TLS_IE64_LO20:
  case R_LARCH_TLS_IE64_HI12:
    return R_GOT;
  case R_LARCH_TLS_LD_HI20:
    return R_TLSLD_GOT;
  case R_LARCH_TLS_GD_HI20:
    return R_TLSGD_GOT;
  case R_LARCH_RELAX:
    // LoongArch linker relaxation is not implemented yet.
    return R_NONE;

  // Other known relocs that are explicitly unimplemented:
  //
  // - psABI v1 relocs that need a stateful stack machine to work, and not
  //   required when implementing psABI v2;
  // - relocs that are not used anywhere (R_LARCH_{ADD,SUB}_24 [1], and the
  //   two GNU vtable-related relocs).
  //
  // [1]: https://github.com/loongson/LoongArch-Documentation/issues/51
  default:
    error(getErrorLocation(loc) + "unknown relocation (" + Twine(type) +
          ") against symbol " + toString(s));
    return R_NONE;
  }
}

bool LoongArch::usesOnlyLowPageBits(RelType type) const {
  switch (type) {
  default:
    return false;
  case R_LARCH_PCALA_LO12:
  case R_LARCH_GOT_LO12:
  case R_LARCH_GOT_PC_LO12:
  case R_LARCH_TLS_IE_PC_LO12:
    return true;
  }
}

void LoongArch::relocate(uint8_t *loc, const Relocation &rel,
                         uint64_t val) const {
  switch (rel.type) {
  case R_LARCH_32_PCREL:
    checkInt(loc, val, 32, rel);
    [[fallthrough]];
  case R_LARCH_32:
  case R_LARCH_TLS_DTPREL32:
    write32le(loc, val);
    return;
  case R_LARCH_64:
  case R_LARCH_TLS_DTPREL64:
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

  // Relocs intended for `addi`, `ld` or `st`.
  case R_LARCH_ABS_LO12:
  case R_LARCH_PCALA_LO12:
  case R_LARCH_GOT_PC_LO12:
  case R_LARCH_GOT_LO12:
  case R_LARCH_TLS_LE_LO12:
  case R_LARCH_TLS_IE_PC_LO12:
  case R_LARCH_TLS_IE_LO12: {
    // Annoyingly, we have to again inspect the insn word to handle the
    // R_LARCH_PCALA_LO12 on JIRL case: JIRL wants its immediate's 2 lowest
    // zeroes removed by us, and the immediate slot width is in fact different.
    // In this case, process like an R_LARCH_B16, but without overflow checking
    // and only taking the value's lowest 12 bits.
    if (rel.type == R_LARCH_PCALA_LO12 && isJirl(read32le(loc))) {
      checkAlignment(loc, val, 4, rel);
      val = SignExtend64<12>(val);
      write32le(loc, setK16(read32le(loc), val >> 2));
      return;
    }
    write32le(loc, setK12(read32le(loc), extractBits(val, 11, 0)));
    return;
  }

  // Relocs intended for `lu12i.w` or `pcalau12i`.
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
    write32le(loc, setJ20(read32le(loc), extractBits(val, 31, 12)));
    return;
  }

  // Relocs intended for `lu32i.d`.
  case R_LARCH_ABS64_LO20:
  case R_LARCH_PCALA64_LO20:
  case R_LARCH_GOT64_PC_LO20:
  case R_LARCH_GOT64_LO20:
  case R_LARCH_TLS_LE64_LO20:
  case R_LARCH_TLS_IE64_PC_LO20:
  case R_LARCH_TLS_IE64_LO20: {
    write32le(loc, setJ20(read32le(loc), extractBits(val, 51, 32)));
    return;
  }

  // Relocs intended for `lu52i.d`.
  case R_LARCH_ABS64_HI12:
  case R_LARCH_PCALA64_HI12:
  case R_LARCH_GOT64_PC_HI12:
  case R_LARCH_GOT64_HI12:
  case R_LARCH_TLS_LE64_HI12:
  case R_LARCH_TLS_IE64_PC_HI12:
  case R_LARCH_TLS_IE64_HI12: {
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

  case R_LARCH_MARK_LA:
  case R_LARCH_MARK_PCREL:
    // no-op
    return;

  case R_LARCH_RELAX:
    return; // Ignored (for now)

  default:
    llvm_unreachable("unknown relocation");
  }
}

TargetInfo *elf::getLoongArchTargetInfo() {
  static LoongArch target;
  return &target;
}
