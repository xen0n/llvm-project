// LoongArchInstrFormats.td - LoongArch Instruction Formats -*- tablegen -*-==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

class InstFormat<bits<5> val> {
  bits<5> Value = val;
}
def InstFormatPseudo : InstFormat<0>;
def InstFormatEmpty  : InstFormat<1>;
def InstFormatRR     : InstFormat<2>;
def InstFormatRRR    : InstFormat<3>;
def InstFormatRRI6   : InstFormat<4>;
def InstFormatRRI8   : InstFormat<5>;
def InstFormatRRI12  : InstFormat<6>;
def InstFormatRRI14  : InstFormat<7>;
def InstFormatRRI16  : InstFormat<8>;
def InstFormatAUI20  : InstFormat<9>;
def InstFormatRI21   : InstFormat<10>;
def InstFormatI25    : InstFormat<11>;
// variants of canonical instruction formats
def InstFormatRRI5   : InstFormat<12>;
def InstFormatRRI6I6 : InstFormat<13>;
def InstFormatRRI5I5 : InstFormat<14>;

class LoongArchOpcode<bits<6> val> {
  bits<6> Value = val;
}
def OPC_BASE  : LoongArchOpcode<0b000000>;
def OPC_AUI   : LoongArchOpcode<0b000101>;
def OPC_AUIPC : LoongArchOpcode<0b000111>;
def OPC_MEM_2 : LoongArchOpcode<0b001001>;
def OPC_MEM   : LoongArchOpcode<0b001010>;
def OPC_BEQZ  : LoongArchOpcode<0b010000>;
def OPC_BNEZ  : LoongArchOpcode<0b010001>;
def OPC_BFP   : LoongArchOpcode<0b010010>;
def OPC_JALR  : LoongArchOpcode<0b010011>;
def OPC_J     : LoongArchOpcode<0b010100>;
def OPC_JAL   : LoongArchOpcode<0b010101>;
def OPC_BEQ   : LoongArchOpcode<0b010110>;
def OPC_BNE   : LoongArchOpcode<0b010111>;
def OPC_BGT   : LoongArchOpcode<0b011000>;
def OPC_BLE   : LoongArchOpcode<0b011001>;
def OPC_BGTU  : LoongArchOpcode<0b011010>;
def OPC_BLEU  : LoongArchOpcode<0b011011>;


class LAInst<dag outs, dag ins, string opcodestr, string argstr,
             list<dag> pattern, InstFormat format>
    : Instruction {
  field bits<32> Inst;
  // SoftFail is a field the disassembler can use to provide a way for
  // instructions to not match without killing the whole decode process. It is
  // mainly used for ARM, but Tablegen expects this field to exist or it fails
  // to build the decode table.
  field bits<32> SoftFail = 0;
  let Size = 4;

  bits<6> Opcode = 0;

  let Inst{31-26} = Opcode;

  let Namespace = "LoongArch";

  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString = opcodestr # "\t" # argstr;
  let Pattern = pattern;

  let TSFlags{4-0} = format.Value;
}

class LAInstRR<LoongArchOpcode opcode, bits<16> sel, dag outs, dag ins,
               string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, [], InstFormatRR> {
  bits<5> rj;
  bits<5> rd;

  let Opcode = opcode.Value;
  let Inst{25-10} = sel;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class LAInstRRR<LoongArchOpcode opcode, bits<11> sel, dag outs, dag ins,
                string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, [], InstFormatRRR> {
  bits<5> rk;
  bits<5> rj;
  bits<5> rd;

  let Opcode = opcode.Value;
  let Inst{25-15} = sel;
  let Inst{14-10} = rk;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class LAInstRRI6<LoongArchOpcode opcode, bits<10> sel, dag outs, dag ins,
                 string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, [], InstFormatRRI6> {
  bits<6> imm6;
  bits<5> rj;
  bits<5> rd;

  let Opcode = opcode.Value;
  let Inst{25-16} = sel;
  let Inst{15-10} = imm6;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class LAInstRRI8<LoongArchOpcode opcode, bits<8> sel, dag outs, dag ins,
                 string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, [], InstFormatRRI8> {
  bits<8> imm8;
  bits<5> rj;
  bits<5> rd;

  let Opcode = opcode.Value;
  let Inst{25-18} = sel;
  let Inst{17-10} = imm8;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class LAInstRRI12<LoongArchOpcode opcode, bits<4> sel, dag outs, dag ins,
                  string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, [], InstFormatRRI12> {
  bits<12> imm12;
  bits<5> rj;
  bits<5> rd;

  let Opcode = opcode.Value;
  let Inst{25-22} = sel;
  let Inst{21-10} = imm12;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class LAInstRRI14<LoongArchOpcode opcode, bits<2> sel, dag outs, dag ins,
                  string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, [], InstFormatRRI14> {
  bits<14> imm14;
  bits<5> rj;
  bits<5> rd;

  let Opcode = opcode.Value;
  let Inst{25-24} = sel;
  let Inst{23-10} = imm14;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class LAInstRRI16<LoongArchOpcode opcode, dag outs, dag ins,
                  string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, [], InstFormatRRI16> {
  bits<16> imm16;
  bits<5> rj;
  bits<5> rd;

  let Opcode = opcode.Value;
  let Inst{25-10} = imm16;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class LAInstAUI20<LoongArchOpcode opcode, bit sel, dag outs, dag ins,
                  string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, [], InstFormatAUI20> {
  bits<20> imm20;
  bits<5> rd;

  let Opcode = opcode.Value;
  let Inst{25} = sel;
  let Inst{24-5} = imm20;
  let Inst{4-0} = rd;
}

class LAInstRI21<LoongArchOpcode opcode, dag outs, dag ins,
                 string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, [], InstFormatRI21> {
  bits<21> imm21;
  bits<5> rj;

  let Opcode = opcode.Value;
  let Inst{25-10} = imm21{15-0};
  let Inst{9-5} = rj;
  let Inst{4-0} = imm21{20-16};
}

class LAInstI25<LoongArchOpcode opcode, dag outs, dag ins,
                 string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, [], InstFormatI25> {
  bits<25> imm25;

  let Opcode = opcode.Value;
  let Inst{25-10} = imm21{15-0};
  let Inst{8-0} = imm21{24-16};
  // XXX all I25 instructions observed have this bit set like this
  let Inst{9} = imm21{24};
}

class LAInstRRI5<LoongArchOpcode opcode, bits<10> sel, dag outs, dag ins,
                 string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, [], InstFormatRRI5> {
  bits<5> imm5;
  bits<5> rj;
  bits<5> rd;

  let Opcode = opcode.Value;
  let Inst{25-16} = sel;
  let Inst{15} = 1;
  let Inst{14-10} = imm5;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class LAInstRRI6I6<LoongArchOpcode opcode, bits<4> sel, dag outs, dag ins,
                 string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, [], InstFormatRRI6I6> {
  bits<6> immb6;
  bits<6> imma6;
  bits<5> rj;
  bits<5> rd;

  let Opcode = opcode.Value;
  let Inst{25-22} = sel;
  let Inst{21-16} = immb6;
  let Inst{15-10} = imma6;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class LAInstRRI5I5<LoongArchOpcode opcode, bits<4> sel, dag outs, dag ins,
                 string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, [], InstFormatRRI5I5> {
  bits<5> immb5;
  bits<5> imma5;
  bits<5> rj;
  bits<5> rd;

  let Opcode = opcode.Value;
  let Inst{25-22} = sel;
  let Inst{21} = 1;
  let Inst{20-16} = immb5;
  let Inst{15} = 1;
  let Inst{14-10} = imma5;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}




