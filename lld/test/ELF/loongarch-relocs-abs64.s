# REQUIRES: loongarch

# RUN: llvm-mc -filetype=obj -triple=loongarch64-unknown-elf %s -o %t.la64.o

# RUN: ld.lld %t.la64.o --defsym foo=0 --defsym bar=42 -o %t.la64.1
# RUN: llvm-objdump -d %t.la64.1 | FileCheck --check-prefix=CASE1 %s
# CASE1:      04 00 00 14   lu12i.w $a0, 0
# CASE1-NEXT: 84 00 80 03   ori     $a0, $a0, 0
# CASE1-NEXT: 04 00 00 16   lu32i.d $a0, 0
# CASE1-NEXT: 84 00 00 03   lu52i.d $a0, $a0, 0
# CASE1-NEXT: 05 00 00 14   lu12i.w $a1, 0
# CASE1-NEXT: a5 a8 80 03   ori     $a1, $a1, 42
# CASE1-NEXT: 05 00 00 16   lu32i.d $a1, 0
# CASE1-NEXT: a5 00 00 03   lu52i.d $a1, $a1, 0

# RUN: ld.lld %t.la64.o --defsym foo=0x12345678 --defsym bar=0x87654321 -o %t.la64.2
# RUN: llvm-objdump -d %t.la64.2 | FileCheck --check-prefix=CASE2 %s
# CASE2:      a4 68 24 14   lu12i.w $a0, 74565
# CASE2-NEXT: 84 e0 99 03   ori     $a0, $a0, 1656
# CASE2-NEXT: 04 00 00 16   lu32i.d $a0, 0
# CASE2-NEXT: 84 00 00 03   lu52i.d $a0, $a0, 0
# CASE2-NEXT: 85 ca 0e 15   lu12i.w $a1, -493996
# CASE2-NEXT: a5 84 8c 03   ori     $a1, $a1, 801
# CASE2-NEXT: 05 00 00 16   lu32i.d $a1, 0
# CASE2-NEXT: a5 00 00 03   lu52i.d $a1, $a1, 0

# RUN: ld.lld %t.la64.o --defsym foo=0x12345fedcb678 --defsym bar=0xfedcb12345000 -o %t.la64.3
# RUN: llvm-objdump -d %t.la64.3 | FileCheck --check-prefix=CASE3 %s
# CASE3:      64 b9 fd 15   lu12i.w $a0, -4661
# CASE3-NEXT: 84 e0 99 03   ori     $a0, $a0, 1656
# CASE3-NEXT: a4 68 24 16   lu32i.d $a0, 74565
# CASE3-NEXT: 84 00 00 03   lu52i.d $a0, $a0, 0
# CASE3-NEXT: a5 68 24 14   lu12i.w $a1, 74565
# CASE3-NEXT: a5 00 80 03   ori     $a1, $a1, 0
# CASE3-NEXT: 65 b9 fd 17   lu32i.d $a1, -4661
# CASE3-NEXT: a5 00 00 03   lu52i.d $a1, $a1, 0

# RUN: ld.lld %t.la64.o --defsym foo=0xfffffeeeeeddd --defsym bar=0xfff00000f1111222 -o %t.la64.4
# RUN: llvm-objdump -d %t.la64.4 | FileCheck --check-prefix=CASE4 %s
# CASE4:      c4 dd dd 15   lu12i.w $a0, -69906
# CASE4-NEXT: 84 74 b7 03   ori     $a0, $a0, 3549
# CASE4-NEXT: e4 ff ff 17   lu32i.d $a0, -1
# CASE4-NEXT: 84 00 00 03   lu52i.d $a0, $a0, 0
# CASE4-NEXT: 25 22 e2 15   lu12i.w $a1, -61167
# CASE4-NEXT: a5 88 88 03   ori     $a1, $a1, 546
# CASE4-NEXT: 05 00 00 16   lu32i.d $a1, 0
# CASE4-NEXT: a5 fc 3f 03   lu52i.d $a1, $a1, -1

.global _start

_start:
    lu12i.w $a0, %abs_hi20(foo)
    ori     $a0, $a0, %abs_lo12(foo)
    lu32i.d $a0, %abs64_lo20(foo)
    lu52i.d $a0, $a0, %abs64_hi12(foo)

    lu12i.w $a1, %abs_hi20(bar)
    ori     $a1, $a1, %abs_lo12(bar)
    lu32i.d $a1, %abs64_lo20(bar)
    lu52i.d $a1, $a1, %abs64_hi12(bar)
