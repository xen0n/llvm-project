//===-- LoongArchELFStreamer.cpp - LoongArch ELF Target Streamer Methods --===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides LoongArch specific target streamer methods.
//
//===----------------------------------------------------------------------===//

#include "LoongArchELFStreamer.h"
#include "LoongArchAsmBackend.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCValue.h"

using namespace llvm;

// This part is for ELF object output.
LoongArchTargetELFStreamer::LoongArchTargetELFStreamer(
    MCStreamer &S, const MCSubtargetInfo &STI)
    : LoongArchTargetStreamer(S) {
  // FIXME: select appropriate ABI.
  setTargetABI(STI.getTargetTriple().isArch64Bit() ? LoongArchABI::ABI_LP64D
                                                   : LoongArchABI::ABI_ILP32D);
}

MCELFStreamer &LoongArchTargetELFStreamer::getStreamer() {
  return static_cast<MCELFStreamer &>(Streamer);
}

void LoongArchTargetELFStreamer::finish() {
  LoongArchTargetStreamer::finish();
  MCAssembler &MCA = getStreamer().getAssembler();
  LoongArchABI::ABI ABI = getTargetABI();

  // FIXME:
  // There are several PRs [1][2][3] that may affect the e_flags.
  // After they got closed or merged, we should update the implementation here
  // accordingly.
  //
  // [1] https://github.com/loongson/LoongArch-Documentation/pull/33
  // [2] https://github.com/loongson/LoongArch-Documentation/pull/47
  // [2] https://github.com/loongson/LoongArch-Documentation/pull/61
  unsigned EFlags = MCA.getELFHeaderEFlags();
  switch (ABI) {
  case LoongArchABI::ABI_ILP32S:
    EFlags |= ELF::EF_LOONGARCH_BASE_ABI_ILP32S;
    break;
  case LoongArchABI::ABI_ILP32F:
    EFlags |= ELF::EF_LOONGARCH_BASE_ABI_ILP32F;
    break;
  case LoongArchABI::ABI_ILP32D:
    EFlags |= ELF::EF_LOONGARCH_BASE_ABI_ILP32D;
    break;
  case LoongArchABI::ABI_LP64S:
    EFlags |= ELF::EF_LOONGARCH_BASE_ABI_LP64S;
    break;
  case LoongArchABI::ABI_LP64F:
    EFlags |= ELF::EF_LOONGARCH_BASE_ABI_LP64F;
    break;
  case LoongArchABI::ABI_LP64D:
    EFlags |= ELF::EF_LOONGARCH_BASE_ABI_LP64D;
    break;
  case LoongArchABI::ABI_Unknown:
    llvm_unreachable("Improperly initialized target ABI");
  }
  MCA.setELFHeaderEFlags(EFlags);
}

namespace {
class LoongArchELFStreamer : public MCELFStreamer {
  static std::pair<unsigned, unsigned> getRelocPairForSize(unsigned Size) {
    switch (Size) {
    default:
      llvm_unreachable("unsupported fixup size");
    case 1:
      return std::make_pair(LoongArch::fixup_loongarch_add_8,
                            LoongArch::fixup_loongarch_sub_8);
    case 2:
      return std::make_pair(LoongArch::fixup_loongarch_add_16,
                            LoongArch::fixup_loongarch_sub_16);
    case 4:
      return std::make_pair(LoongArch::fixup_loongarch_add_32,
                            LoongArch::fixup_loongarch_sub_32);
    case 8:
      return std::make_pair(LoongArch::fixup_loongarch_add_64,
                            LoongArch::fixup_loongarch_sub_64);
    }
  }

  static bool requiresFixups(MCContext &C, const MCExpr *Value,
                             const MCExpr *&LHS, const MCExpr *&RHS) {
    auto IsMetadataOrEHFrameSection = [](const MCSection &S) -> bool {
      // Additionally check .apple_names/.apple_types. They are fixed-size and
      // do not need fixups. llvm-dwarfdump --apple-names does not process
      // R_LARCH_{ADD,SUB}32 in them.
      return S.getKind().isMetadata() || S.getName() == ".eh_frame" ||
             S.getName() == ".apple_names" || S.getName() == ".apple_types";
    };

    const auto *MBE = dyn_cast<MCBinaryExpr>(Value);
    if (MBE == nullptr)
      return false;

    MCValue E;
    if (!Value->evaluateAsRelocatable(E, nullptr, nullptr))
      return false;
    if (E.getSymA() == nullptr || E.getSymB() == nullptr)
      return false;

    const auto &A = E.getSymA()->getSymbol();
    const auto &B = E.getSymB()->getSymbol();

    LHS =
        MCBinaryExpr::create(MCBinaryExpr::Add, MCSymbolRefExpr::create(&A, C),
                             MCConstantExpr::create(E.getConstant(), C), C);
    RHS = E.getSymB();

    // TODO: when available, R_RISCV_n_PCREL should be preferred.

    // Avoid pairwise relocations for symbolic difference in debug and .eh_frame
    if (A.isInSection())
      return !IsMetadataOrEHFrameSection(A.getSection());
    if (B.isInSection())
      return !IsMetadataOrEHFrameSection(B.getSection());
    // as well as for absolute symbols.
    return !A.getName().empty() || !B.getName().empty();
  }

public:
  LoongArchELFStreamer(MCContext &C, std::unique_ptr<MCAsmBackend> MAB,
                       std::unique_ptr<MCObjectWriter> MOW,
                       std::unique_ptr<MCCodeEmitter> MCE)
      : MCELFStreamer(C, std::move(MAB), std::move(MOW), std::move(MCE)) {}

  void emitValueImpl(const MCExpr *Value, unsigned Size, SMLoc Loc) override {
    const MCExpr *A, *B;
    if (!requiresFixups(getContext(), Value, A, B))
      return MCELFStreamer::emitValueImpl(Value, Size, Loc);

    MCStreamer::emitValueImpl(Value, Size, Loc);

    MCDataFragment *DF = getOrCreateDataFragment();
    flushPendingLabels(DF, DF->getContents().size());
    MCDwarfLineEntry::make(this, getCurrentSectionOnly());

    unsigned Add, Sub;
    std::tie(Add, Sub) = getRelocPairForSize(Size);

    DF->getFixups().push_back(MCFixup::create(
        DF->getContents().size(), A, static_cast<MCFixupKind>(Add), Loc));
    DF->getFixups().push_back(MCFixup::create(
        DF->getContents().size(), B, static_cast<MCFixupKind>(Sub), Loc));

    DF->getContents().resize(DF->getContents().size() + Size, 0);
  }
};
} // end namespace

namespace llvm {
MCELFStreamer *createLoongArchELFStreamer(MCContext &C,
                                          std::unique_ptr<MCAsmBackend> MAB,
                                          std::unique_ptr<MCObjectWriter> MOW,
                                          std::unique_ptr<MCCodeEmitter> MCE,
                                          bool RelaxAll) {
  LoongArchELFStreamer *S = new LoongArchELFStreamer(
      C, std::move(MAB), std::move(MOW), std::move(MCE));
  S->getAssembler().setRelaxAll(RelaxAll);
  return S;
}
} // end namespace llvm
