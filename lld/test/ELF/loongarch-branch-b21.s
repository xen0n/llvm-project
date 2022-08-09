# REQUIRES: loongarch

# RUN: llvm-mc --filetype=obj --triple=loongarch32-unknown-elf %s -o %t.la32.o
# RUN: llvm-mc --filetype=obj --triple=loongarch64-unknown-elf %s -o %t.la64.o

# RUN: ld.lld %t.la32.o --defsym foo=_start+4 --defsym bar=_start -o %t.la32
# RUN: ld.lld %t.la64.o --defsym foo=_start+4 --defsym bar=_start -o %t.la64
# RUN: llvm-objdump -d %t.la32 | FileCheck %s --check-prefix=CHECK-32
# RUN: llvm-objdump -d %t.la64 | FileCheck %s --check-prefix=CHECK-64
# CHECK-32: e0 07 00 40   beqz    $s8, 4
# CHECK-32: ff ff ff 47   bnez    $s8, -4
# CHECK-64: e0 07 00 40   beqz    $s8, 4
# CHECK-64: ff ff ff 47   bnez    $s8, -4

# RUN: ld.lld %t.la32.o --defsym foo=_start+0x3ffffc --defsym bar=_start+4-0x400000 -o %t.la32.limits
# RUN: ld.lld %t.la64.o --defsym foo=_start+0x3ffffc --defsym bar=_start+4-0x400000 -o %t.la64.limits
# RUN: llvm-objdump -d %t.la32.limits | FileCheck --check-prefix=LIMITS-32 %s
# RUN: llvm-objdump -d %t.la64.limits | FileCheck --check-prefix=LIMITS-64 %s
# LIMITS-32:      ef ff ff 43   beqz    $s8, 4194300
# LIMITS-32-NEXT: f0 03 00 44   bnez    $s8, -4194304
# LIMITS-64:      ef ff ff 43   beqz    $s8, 4194300
# LIMITS-64-NEXT: f0 03 00 44   bnez    $s8, -4194304

# RUN: not ld.lld %t.la32.o --defsym foo=_start+0x400000 --defsym bar=_start+4-0x400004 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-RANGE %s
# RUN: not ld.lld %t.la64.o --defsym foo=_start+0x400000 --defsym bar=_start+4-0x400004 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-RANGE %s
# ERROR-RANGE: relocation R_LARCH_B21 out of range: 4194304 is not in [-4194304, 4194303]; references 'foo'
# ERROR-RANGE: relocation R_LARCH_B21 out of range: -4194308 is not in [-4194304, 4194303]; references 'bar'

# RUN: not ld.lld %t.la32.o --defsym foo=_start+1 --defsym bar=_start-1 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-ALIGN-1 %s
# RUN: not ld.lld %t.la64.o --defsym foo=_start+1 --defsym bar=_start-1 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-ALIGN-1 %s
# ERROR-ALIGN-1: improper alignment for relocation R_LARCH_B21: 0xFFFFFFFFFFFFFFFB is not aligned to 4 bytes

# RUN: not ld.lld %t.la32.o --defsym foo=_start+2 --defsym bar=_start-2 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-ALIGN-2 %s
# RUN: not ld.lld %t.la64.o --defsym foo=_start+2 --defsym bar=_start-2 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-ALIGN-2 %s
# ERROR-ALIGN-2: improper alignment for relocation R_LARCH_B21: 0xFFFFFFFFFFFFFFFA is not aligned to 4 bytes

.global _start
_start:
     beqz $s8, foo
     bnez $s8, bar
