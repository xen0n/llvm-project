//===-- LoongArchTargetInfo.cpp - LoongArch Target Implementation ---------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "TargetInfo/LoongArchTargetInfo.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

Target &llvm::getTheLoongArch64Target() {
  static Target TheLoongArch64Target;
  return TheLoongArch64Target;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeLoongArchTargetInfo() {
  RegisterTarget<Triple::loongarch64> X(getTheLoongArch64Target(),
                                        "loongarch64", "64-bit LoongArch", "LoongArch");
}

// FIXME: Temporary stub - this function must be defined for linking
// to succeed and will be called unconditionally by llc, so must be a no-op.
// Remove once this function is properly implemented.
extern "C" void LLVMInitializeLoongArchTargetMC() {}
