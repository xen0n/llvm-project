# REQUIRES: loongarch

# RUN: llvm-mc --filetype=obj --triple=loongarch32-unknown-elf %s -o %t.la32.o
# RUN: llvm-mc --filetype=obj --triple=loongarch64-unknown-elf %s -o %t.la64.o

# RUN: ld.lld %t.la32.o --defsym foo=_start+4 --defsym bar=_start -o %t.la32
# RUN: ld.lld %t.la64.o --defsym foo=_start+4 --defsym bar=_start -o %t.la64
# RUN: llvm-objdump --no-show-raw-insn -d %t.la32 | FileCheck %s --check-prefix=CHECK
# RUN: llvm-objdump --no-show-raw-insn -d %t.la64 | FileCheck %s --check-prefix=CHECK
# CHECK: beq $zero, $zero, 4
# CHECK: bne $zero, $zero, -4

# RUN: ld.lld %t.la32.o --defsym foo=_start+0x1fffc --defsym bar=_start+4-0x20000 -o %t.la32.limits
# RUN: ld.lld %t.la64.o --defsym foo=_start+0x1fffc --defsym bar=_start+4-0x20000 -o %t.la64.limits
# RUN: llvm-objdump --no-show-raw-insn -d %t.la32.limits | FileCheck --check-prefix=LIMITS %s
# RUN: llvm-objdump --no-show-raw-insn -d %t.la64.limits | FileCheck --check-prefix=LIMITS %s
# LIMITS:      beq $zero, $zero, 131068
# LIMITS-NEXT: bne $zero, $zero, -131072

# RUN: not ld.lld %t.la32.o --defsym foo=_start+0x20000 --defsym bar=_start+4-0x20004 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-RANGE %s
# RUN: not ld.lld %t.la64.o --defsym foo=_start+0x20000 --defsym bar=_start+4-0x20004 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-RANGE %s
# ERROR-RANGE: relocation R_LARCH_B16 out of range: 131072 is not in [-131072, 131071]; references 'foo'
# ERROR-RANGE: relocation R_LARCH_B16 out of range: -131076 is not in [-131072, 131071]; references 'bar'

# RUN: not ld.lld %t.la32.o --defsym foo=_start+1 --defsym bar=_start-1 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-ALIGN-1 %s
# RUN: not ld.lld %t.la64.o --defsym foo=_start+1 --defsym bar=_start-1 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-ALIGN-1 %s
# ERROR-ALIGN-1: improper alignment for relocation R_LARCH_B16: 0xFFFFFFFFFFFFFFFB is not aligned to 4 bytes

# RUN: not ld.lld %t.la32.o --defsym foo=_start+2 --defsym bar=_start-2 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-ALIGN-2 %s
# RUN: not ld.lld %t.la64.o --defsym foo=_start+2 --defsym bar=_start-2 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-ALIGN-2 %s
# ERROR-ALIGN-2: improper alignment for relocation R_LARCH_B16: 0xFFFFFFFFFFFFFFFA is not aligned to 4 bytes

.global _start
_start:
     beq $zero, $zero, foo
     bne $zero, $zero, bar
