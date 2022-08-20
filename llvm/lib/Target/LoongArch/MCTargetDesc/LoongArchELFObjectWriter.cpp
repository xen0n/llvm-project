//===-- LoongArchELFObjectWriter.cpp - LoongArch ELF Writer ---*- C++ -*---===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/LoongArchFixupKinds.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {
class LoongArchELFObjectWriter : public MCELFObjectTargetWriter {
public:
  LoongArchELFObjectWriter(uint8_t OSABI, bool Is64Bit);

  ~LoongArchELFObjectWriter() override;

  // Return true if the given relocation must be with a symbol rather than
  // section plus offset.
  bool needsRelocateWithSymbol(const MCSymbol &Sym,
                               unsigned Type) const override {
    return true;
  }

protected:
  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const override;
};
} // end namespace

LoongArchELFObjectWriter::LoongArchELFObjectWriter(uint8_t OSABI, bool Is64Bit)
    : MCELFObjectTargetWriter(Is64Bit, OSABI, ELF::EM_LOONGARCH,
                              /*HasRelocationAddend*/ true) {}

LoongArchELFObjectWriter::~LoongArchELFObjectWriter() {}

unsigned LoongArchELFObjectWriter::getRelocType(MCContext &Ctx,
                                                const MCValue &Target,
                                                const MCFixup &Fixup,
                                                bool IsPCRel) const {
  // Determine the type of the relocation
  unsigned Kind = Fixup.getTargetKind();

  if (Kind >= FirstLiteralRelocationKind)
    return Kind - FirstLiteralRelocationKind;

  switch (Kind) {
  default:
    Ctx.reportError(Fixup.getLoc(), "Unsupported relocation type");
    return ELF::R_LARCH_NONE;
  case FK_Data_1:
    Ctx.reportError(Fixup.getLoc(), "1-byte data relocations not supported");
    return ELF::R_LARCH_NONE;
  case FK_Data_2:
    Ctx.reportError(Fixup.getLoc(), "2-byte data relocations not supported");
    return ELF::R_LARCH_NONE;
  case FK_Data_4:
    return ELF::R_LARCH_32;
  case FK_Data_8:
    return ELF::R_LARCH_64;
  case LoongArch::fixup_loongarch_pcala_hi20:
    return ELF::R_LARCH_PCALA_HI20;
  case LoongArch::fixup_loongarch_pcala_lo12:
    return ELF::R_LARCH_PCALA_LO12;
  case LoongArch::fixup_loongarch_b26:
    return ELF::R_LARCH_B26;
  case LoongArch::fixup_loongarch_add_8:
    return ELF::R_LARCH_ADD8;
  case LoongArch::fixup_loongarch_sub_8:
    return ELF::R_LARCH_SUB8;
  case LoongArch::fixup_loongarch_add_16:
    return ELF::R_LARCH_ADD16;
  case LoongArch::fixup_loongarch_sub_16:
    return ELF::R_LARCH_SUB16;
  case LoongArch::fixup_loongarch_add_32:
    return ELF::R_LARCH_ADD32;
  case LoongArch::fixup_loongarch_sub_32:
    return ELF::R_LARCH_SUB32;
  case LoongArch::fixup_loongarch_add_64:
    return ELF::R_LARCH_ADD64;
  case LoongArch::fixup_loongarch_sub_64:
    return ELF::R_LARCH_SUB64;
    // TODO: Handle more fixup-kinds.
  }
}

std::unique_ptr<MCObjectTargetWriter>
llvm::createLoongArchELFObjectWriter(uint8_t OSABI, bool Is64Bit) {
  return std::make_unique<LoongArchELFObjectWriter>(OSABI, Is64Bit);
}
