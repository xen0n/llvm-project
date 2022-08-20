//===- LoongArchFixupKinds.h - LoongArch Specific Fixup Entries -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LOONGARCH_MCTARGETDESC_LOONGARCHFIXUPKINDS_H
#define LLVM_LIB_TARGET_LOONGARCH_MCTARGETDESC_LOONGARCHFIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

#undef LoongArch

namespace llvm {
namespace LoongArch {
//
// This table *must* be in the same order of
// MCFixupKindInfo Infos[LoongArch::NumTargetFixupKinds] in
// LoongArchAsmBackend.cpp.
//
enum Fixups {
  // 26-bit fixup for symbol references in the b/bl instructions.
  fixup_loongarch_b26 = FirstTargetFixupKind,
  // 20-bit fixup corresponding to %pc_hi20(foo) for instruction pcalau12i.
  fixup_loongarch_pcala_hi20,
  // 12-bit fixup corresponding to %pc_lo12(foo) for instructions addi.w/d.
  fixup_loongarch_pcala_lo12,
  // 8-bit fixup corresponding to R_LARCH_ADD8 for 8-bit symbolic difference
  // paired relocations.
  fixup_loongarch_add_8,
  // 8-bit fixup corresponding to R_LARCH_SUB8 for 8-bit symbolic difference
  // paired relocations.
  fixup_loongarch_sub_8,
  // 16-bit fixup corresponding to R_LARCH_ADD16 for 16-bit symbolic difference
  // paired reloctions.
  fixup_loongarch_add_16,
  // 16-bit fixup corresponding to R_LARCH_SUB16 for 16-bit symbolic difference
  // paired reloctions.
  fixup_loongarch_sub_16,
  // 32-bit fixup corresponding to R_LARCH_ADD32 for 32-bit symbolic difference
  // paired relocations.
  fixup_loongarch_add_32,
  // 32-bit fixup corresponding to R_LARCH_SUB32 for 32-bit symbolic difference
  // paired relocations.
  fixup_loongarch_sub_32,
  // 64-bit fixup corresponding to R_LARCH_ADD64 for 64-bit symbolic difference
  // paired relocations.
  fixup_loongarch_add_64,
  // 64-bit fixup corresponding to R_LARCH_SUB64 for 64-bit symbolic difference
  // paired relocations.
  fixup_loongarch_sub_64,
  // TODO: Add more fixup kind.

  // Used as a sentinel, must be the last.
  fixup_loongarch_invalid,
  NumTargetFixupKinds = fixup_loongarch_invalid - FirstTargetFixupKind
};
} // end namespace LoongArch
} // end namespace llvm

#endif
