# REQUIRES: loongarch

# RUN: llvm-mc -filetype=obj -triple=loongarch64-unknown-elf %s -o %t.la64.o

# RUN: ld.lld %t.la64.o --defsym foo=_start+4 --defsym bar=_start -o %t.la64
# RUN: llvm-objdump -d %t.la64 | FileCheck %s --check-prefix=CHECK-64
# CHECK-64: ff 07 00 58   beq     $s8, $s8, 4
# CHECK-64: ff ff ff 5f   bne     $s8, $s8, -4
#
# RUN: ld.lld %t.la64.o --defsym foo=_start+0xffe --defsym bar=_start+4-0x1000 -o %t.la64.limits
# RUN: llvm-objdump -d %t.la64.limits | FileCheck --check-prefix=LIMITS-64 %s
# LIMITS-64:      e3 0f 00 7e     beqz    zero, 0x1211e
# LIMITS-64-NEXT: 63 10 00 80     bnez    zero, 0x10124

# RUN: not ld.lld %t.la64.o --defsym foo=_start+0x1000 --defsym bar=_start+4-0x1002 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-RANGE %s
# ERROR-RANGE: relocation R_RISCV_BRANCH out of range: 4096 is not in [-4096, 4095]; references foo
# ERROR-RANGE: relocation R_RISCV_BRANCH out of range: -4098 is not in [-4096, 4095]; references bar

# RUN: not ld.lld %t.la64.o --defsym foo=_start+1 --defsym bar=_start-1 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-ALIGN %s
# ERROR-ALIGN: improper alignment for relocation R_RISCV_BRANCH: 0x1 is not aligned to 2 bytes

.global _start
_start:
     beq $s8, $s8, foo
     bne $s8, $s8, bar
