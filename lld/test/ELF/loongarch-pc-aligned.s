# REQUIRES: loongarch
# RUN: rm -rf %t && split-file %s %t

# RUN: llvm-mc --filetype=obj --triple=loongarch32 %t/a.s -o %t/a.la32.o
# RUN: llvm-mc --filetype=obj --triple=loongarch64 %t/a.s -o %t/a.la64.o
# RUN: llvm-mc --filetype=obj --triple=loongarch64 %t/extreme.s -o %t/extreme.o

# RUN: ld.lld %t/a.la32.o -T %t/case1.t -o %t/case1.la32
# RUN: ld.lld %t/a.la64.o -T %t/case1.t -o %t/case1.la64
# RUN: llvm-objdump -d --no-show-raw-insn %t/case1.la32 | FileCheck %s --check-prefix=CASE1
# RUN: llvm-objdump -d --no-show-raw-insn %t/case1.la64 | FileCheck %s --check-prefix=CASE1
# CASE1:      pcalau12i $a0, 0
# CASE1-NEXT: ld.w      $a0, $a0, 0
#--- case1.t
SECTIONS {
 .rodata 0x1000: { *(.rodata) }
 .text   0x1ffc: { *(.text) }
}

# RUN: ld.lld %t/a.la32.o -T %t/case2.t -o %t/case2.la32
# RUN: ld.lld %t/a.la64.o -T %t/case2.t -o %t/case2.la64
# RUN: llvm-objdump -d --no-show-raw-insn %t/case2.la32 | FileCheck %s --check-prefix=CASE2
# RUN: llvm-objdump -d --no-show-raw-insn %t/case2.la64 | FileCheck %s --check-prefix=CASE2
# CASE2:      pcalau12i $a0, -1
# CASE2-NEXT: ld.w      $a0, $a0, 0
#--- case2.t
SECTIONS {
 .rodata 0x1000: { *(.rodata) }
 .text   0x2000: { *(.text) }
}

# RUN: ld.lld %t/a.la32.o -T %t/case3.t -o %t/case3.la32
# RUN: ld.lld %t/a.la64.o -T %t/case3.t -o %t/case3.la64
# RUN: llvm-objdump -d --no-show-raw-insn %t/case3.la32 | FileCheck %s --check-prefix=CASE3
# RUN: llvm-objdump -d --no-show-raw-insn %t/case3.la64 | FileCheck %s --check-prefix=CASE3
# CASE3:      pcalau12i $a0, -1
# CASE3-NEXT: ld.w      $a0, $a0, 2047
#--- case3.t
SECTIONS {
 .rodata 0x17ff: { *(.rodata) }
 .text   0x2000: { *(.text) }
}

# RUN: ld.lld %t/a.la32.o -T %t/case4.t -o %t/case4.la32
# RUN: ld.lld %t/a.la64.o -T %t/case4.t -o %t/case4.la64
# RUN: llvm-objdump -d --no-show-raw-insn %t/case4.la32 | FileCheck %s --check-prefix=CASE4
# RUN: llvm-objdump -d --no-show-raw-insn %t/case4.la64 | FileCheck %s --check-prefix=CASE4
# CASE4:      pcalau12i $a0, 0
# CASE4-NEXT: ld.w      $a0, $a0, -2048
#--- case4.t
SECTIONS {
 .rodata 0x1800: { *(.rodata) }
 .text   0x2000: { *(.text) }
}

# RUN: ld.lld %t/a.la32.o -T %t/case5.t -o %t/case5.la32
# RUN: ld.lld %t/a.la64.o -T %t/case5.t -o %t/case5.la64
# RUN: llvm-objdump -d --no-show-raw-insn %t/case5.la32 | FileCheck %s --check-prefix=CASE5
# RUN: llvm-objdump -d --no-show-raw-insn %t/case5.la64 | FileCheck %s --check-prefix=CASE5
# CASE5:      pcalau12i $a0, 1
# CASE5-NEXT: ld.w      $a0, $a0, 4
#--- case5.t
SECTIONS {
 .text   0x001ffc: { *(.text) }
 .rodata 0x002004: { *(.rodata) }
}

# RUN: ld.lld %t/a.la32.o -T %t/case6.t -o %t/case6.la32
# RUN: ld.lld %t/a.la64.o -T %t/case6.t -o %t/case6.la64
# RUN: llvm-objdump -d --no-show-raw-insn %t/case6.la32 | FileCheck %s --check-prefix=CASE6
# RUN: llvm-objdump -d --no-show-raw-insn %t/case6.la64 | FileCheck %s --check-prefix=CASE6
# CASE6:      pcalau12i $a0, 2
# CASE6-NEXT: ld.w      $a0, $a0, -2048
#--- case6.t
SECTIONS {
 .text   0x001ffc: { *(.text) }
 .rodata 0x002800: { *(.rodata) }
}

# RUN: ld.lld %t/a.la32.o -T %t/case7.t -o %t/case7.la32
# RUN: ld.lld %t/a.la64.o -T %t/case7.t -o %t/case7.la64
# RUN: llvm-objdump -d --no-show-raw-insn %t/case7.la32 | FileCheck %s --check-prefix=CASE7
# RUN: llvm-objdump -d --no-show-raw-insn %t/case7.la64 | FileCheck %s --check-prefix=CASE7
# CASE7:      pcalau12i $a0, 524287
# CASE7-NEXT: ld.w      $a0, $a0, 291
#--- case7.t
SECTIONS {
 .text   0x0: { *(.text) }
 .rodata 0x7ffff123: { *(.rodata) }
}

# RUN: ld.lld %t/a.la32.o -T %t/case8.t -o %t/case8.la32
# RUN: ld.lld %t/a.la64.o -T %t/case8.t -o %t/case8.la64
# RUN: llvm-objdump -d --no-show-raw-insn %t/case8.la32 | FileCheck %s --check-prefix=CASE8
# RUN: llvm-objdump -d --no-show-raw-insn %t/case8.la64 | FileCheck %s --check-prefix=CASE8
# CASE8:      pcalau12i $a0, -524288
# CASE8-NEXT: ld.w      $a0, $a0, -1348
#--- case8.t
SECTIONS {
 .text   0x0: { *(.text) }
 .rodata 0x7ffffabc: { *(.rodata) }
}

# RUN: ld.lld %t/a.la32.o -T %t/case9.t -o %t/case9.la32
# RUN: ld.lld %t/a.la64.o -T %t/case9.t -o %t/case9.la64
# RUN: llvm-objdump -d --no-show-raw-insn %t/case9.la32 | FileCheck %s --check-prefix=CASE9
# RUN: llvm-objdump -d --no-show-raw-insn %t/case9.la64 | FileCheck %s --check-prefix=CASE9
# CASE9:      pcalau12i $a0, -524288
# CASE9-NEXT: ld.w      $a0, $a0, 291
#--- case9.t
SECTIONS {
 .rodata 0x123: { *(.rodata) }
 .text   0x80000000: { *(.text) }
}

# RUN: ld.lld %t/extreme.o -T %t/extreme1.t -o %t/extreme1
# RUN: llvm-objdump -d --no-show-raw-insn %t/extreme1 | FileCheck %s --check-prefix=EXTREME1
# EXTREME1:      addi.d    $t0, $zero, 291
# EXTREME1-NEXT: lu32i.d   $t0, -284281
# EXTREME1-NEXT: lu52i.d   $t0, $t0, -292
# EXTREME1-NEXT: pcalau12i $t1, 414771
#--- extreme1.t
SECTIONS {
 .rodata 0x123: { *(.rodata) }
 .text   0x123456789abcdef0: { *(.text) }
}

# RUN: ld.lld %t/extreme.o -T %t/extreme2.t -o %t/extreme2
# RUN: llvm-objdump -d --no-show-raw-insn %t/extreme2 | FileCheck %s --check-prefix=EXTREME2
# EXTREME2:      addi.d    $t0, $zero, -820
# EXTREME2-NEXT: lu32i.d   $t0, -69907
# EXTREME2-NEXT: lu52i.d   $t0, $t0, 2047
# EXTREME2-NEXT: pcalau12i $t1, -139810
#--- extreme2.t
SECTIONS {
 .text   0x0: { *(.text) }
 .rodata 0x7ffeeeeedddddccc: { *(.rodata) }
}

# RUN: ld.lld %t/extreme.o -T %t/extreme3.t -o %t/extreme3
# RUN: llvm-objdump -d --no-show-raw-insn %t/extreme3 | FileCheck %s --check-prefix=EXTREME3
# EXTREME3:      addi.d    $t0, $zero, -820
# EXTREME3-NEXT: lu32i.d   $t0, -69906
# EXTREME3-NEXT: lu52i.d   $t0, $t0, -2048
# EXTREME3-NEXT: pcalau12i $t1, -139810
#--- extreme3.t
SECTIONS {
 .text   0x0: { *(.text) }
 .rodata 0x800eeeeedddddccc: { *(.rodata) }
}

#--- a.s
.rodata
x:
.word 10
.text
.global _start
_start:
  pcalau12i $a0, %pc_hi20(x)
  ld.w      $a0, $a0, %pc_lo12(x)

#--- extreme.s
.rodata
x:
.word 10
.text
.global _start
_start:
  addi.d    $t0, $zero, %pc_lo12(x)
  lu32i.d   $t0, %pc64_lo20(x)
  lu52i.d   $t0, $t0, %pc64_hi12(x)
  pcalau12i $t1, %pc_hi20(x)
  add.d     $t0, $t0, $t1
