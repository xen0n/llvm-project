//=== LoongArchTargetMachine.cpp - Define TargetMachine for LoongArch -----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Implements the info about LoongArch target spec.
//
//===----------------------------------------------------------------------===//

#include "LoongArchTargetMachine.h"
#include "TargetInfo/LoongArchTargetInfo.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeLoongArchTarget() {
  RegisterTargetMachine<LoongArchTargetMachine> X(getTheLoongArchTarget());
}

static std::string computeDataLayout(const Triple &TT) {
  assert(TT.isArch64Bit() && "only LoongArch64 is currently supported");
  return "e-m:e-p:64:64-i64:64-i128:128-n64-S128";
}

LoongArchTargetMachine::LoongArchTargetMachine(const Target &T, const Triple &TT,
                                     StringRef CPU, StringRef FS,
                                     const TargetOptions &Options,
                                     Optional<Reloc::Model> RM,
                                     Optional<CodeModel::Model> CM,
                                     CodeGenOpt::Level OL, bool JIT)
    : LLVMTargetMachine(T, computeDataLayout(TT), TT, CPU, FS, Options,
                        RM.getValueOr(Reloc::Static),
                        getEffectiveCodeModel(CM, CodeModel::Small), OL),
      TLOF(std::make_unique<TargetLoweringObjectFileELF>()) {
  initAsmInfo();
}

namespace {
class LoongArchPassConfig : public TargetPassConfig {
public:
  LoongArchPassConfig(LoongArchTargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  LoongArchTargetMachine &getLoongArchTargetMachine() const {
    return getTM<LoongArchTargetMachine>();
  }
};

} // namespace

TargetPassConfig *LoongArchTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new LoongArchPassConfig(*this, PM);
}
