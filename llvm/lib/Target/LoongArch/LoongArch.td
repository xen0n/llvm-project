//=== LoongArch.td - Describe the LoongArch Target Machine -*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// Registers, calling conventions, instruction descriptions.
//===----------------------------------------------------------------------===//

include "LoongArchRegisterInfo.td"
include "LoongArchInstrInfo.td"

//===----------------------------------------------------------------------===//
// LoongArch processors supported.
//===----------------------------------------------------------------------===//

def : ProcessorModel<"generic-loongarch64", NoSchedModel, []>;

//===----------------------------------------------------------------------===//
// Define the LoongArch target.
//===----------------------------------------------------------------------===//

def LoongArchInstrInfo : InstrInfo;

def LoongArch : Target {
  let InstructionSet = LoongArchInstrInfo;
}
