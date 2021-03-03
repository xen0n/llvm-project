//=== LoongArchRegisterInfo.td - LoongArch Register defs ---*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Declarations that describe the LoongArch registers.
//===----------------------------------------------------------------------===//

let Namespace = "LoongArch" in {
  class LoongArchReg<bits<5> Enc, string n, list<string> alt = []> : Register<n> {
    let HWEncoding{4 - 0} = Enc;
    let AltNames = alt;
  }

  class LoongArchReg32<bits<5> Enc, string n, list<string> alt = []> : Register<n> {
    let HWEncoding{4 - 0} = Enc;
    let AltNames = alt;
  }

  // Because LoongArchReg64 register have AsmName and AltNames that alias with their
  // 32-bit sub-register, LoongArchAsmParser will need to coerce a register number
  // from a LoongArchReg32 to the equivalent LoongArchReg64 when appropriate.
  def sub_32 : SubRegIndex<32>;
  class LoongArchReg64<LoongArchReg32 subreg> : Register<""> {
    let HWEncoding{4-0} = subreg.HWEncoding{4-0};
    let SubRegs = [subreg];
    let SubRegIndices = [sub_32];
    let AsmName = subreg.AsmName;
    let AltNames = subreg.AltNames;
  }

  class LoongArchRegWithSubRegs<bits<5> Enc, string n, list<Register> subregs,
                                list<string> alt = []>
        : RegisterWithSubRegs<n, subregs> {
    let HWEncoding{4-0} = Enc;
    let AltNames = alt;
  }

  def ABIRegAltName : RegAltNameIndex;
} // Namespace = "LoongArch"


let RegAltNameIndices = [ABIRegAltName] in {
  def R0  : LoongArchReg<0,  "r0",  ["zero"]>, DwarfRegNum<[0]>;
  def R1  : LoongArchReg<1,  "r1",  ["ra"]>, DwarfRegNum<[1]>;
  def R2  : LoongArchReg<2,  "r2",  []>, DwarfRegNum<[2]>;
  def R3  : LoongArchReg<3,  "r3",  ["sp"]>, DwarfRegNum<[3]>;
  def R4  : LoongArchReg<4,  "r4",  ["a0"]>, DwarfRegNum<[4]>;
  def R5  : LoongArchReg<5,  "r5",  ["a1"]>, DwarfRegNum<[5]>;
  def R6  : LoongArchReg<6,  "r6",  ["a2"]>, DwarfRegNum<[6]>;
  def R7  : LoongArchReg<7,  "r7",  ["a3"]>, DwarfRegNum<[7]>;
  def R8  : LoongArchReg<8,  "r8",  ["a4"]>, DwarfRegNum<[8]>;
  def R9  : LoongArchReg<9,  "r9",  ["a5"]>, DwarfRegNum<[9]>;
  def R10 : LoongArchReg<10, "r10", ["a6"]>, DwarfRegNum<[10]>;
  def R11 : LoongArchReg<11, "r11", ["a7"]>, DwarfRegNum<[11]>;
  def R12 : LoongArchReg<12, "r12", ["t0"]>, DwarfRegNum<[12]>;
  def R13 : LoongArchReg<13, "r13", ["t1"]>, DwarfRegNum<[13]>;
  def R14 : LoongArchReg<14, "r14", ["t2"]>, DwarfRegNum<[14]>;
  def R15 : LoongArchReg<15, "r15", ["t3"]>, DwarfRegNum<[15]>;
  def R16 : LoongArchReg<16, "r16", ["t4"]>, DwarfRegNum<[16]>;
  def R17 : LoongArchReg<17, "r17", ["t5"]>, DwarfRegNum<[17]>;
  def R18 : LoongArchReg<18, "r18", ["t6"]>, DwarfRegNum<[18]>;
  def R19 : LoongArchReg<19, "r19", ["t7"]>, DwarfRegNum<[19]>;
  def R20 : LoongArchReg<20, "r20", []>, DwarfRegNum<[20]>;
  def R21 : LoongArchReg<21, "r21", []>, DwarfRegNum<[21]>;
  def R22 : LoongArchReg<22, "r22", ["s9", "fp"]>, DwarfRegNum<[22]>;
  def R23 : LoongArchReg<23, "r23", ["s0"]>, DwarfRegNum<[23]>;
  def R24 : LoongArchReg<24, "r24", ["s1"]>, DwarfRegNum<[24]>;
  def R25 : LoongArchReg<25, "r25", ["s2"]>, DwarfRegNum<[25]>;
  def R26 : LoongArchReg<26, "r26", ["s3"]>, DwarfRegNum<[26]>;
  def R27 : LoongArchReg<27, "r27", ["s4"]>, DwarfRegNum<[27]>;
  def R28 : LoongArchReg<28, "r28", ["s5"]>, DwarfRegNum<[28]>;
  def R29 : LoongArchReg<29, "r29", ["s6"]>, DwarfRegNum<[29]>;
  def R30 : LoongArchReg<30, "r30", ["s7"]>, DwarfRegNum<[30]>;
  def R31 : LoongArchReg<31, "r31", ["s8"]>, DwarfRegNum<[31]>;
}

//===----------------------------------------------------------------------===//
// Declarations that describe the LoongArch register class.
//===----------------------------------------------------------------------===//

// The order of registers represents the preferred allocation sequence.
// Registers are listed in the order caller-save, callee-save, specials.
def GPR : RegisterClass<"LoongArch", [i64], 32, (add
    (sequence "R%u", 4, 11),  // a0-a7
    (sequence "R%u", 12, 19), // t0-t7
    (sequence "R%u", 23, 31), // s0-s8
    R22,                      // s9
    (sequence "R%u", 0, 3),   // zero-sp
    R20, R21                  // presently unknown
  )> {
  let Size = 64;
}
