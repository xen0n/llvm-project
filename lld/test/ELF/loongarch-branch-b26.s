# REQUIRES: loongarch

# RUN: llvm-mc --filetype=obj --triple=loongarch32-unknown-elf %s -o %t.la32.o
# RUN: llvm-mc --filetype=obj --triple=loongarch64-unknown-elf %s -o %t.la64.o

# RUN: ld.lld %t.la32.o --defsym foo=_start+4 --defsym bar=_start -o %t.la32
# RUN: ld.lld %t.la64.o --defsym foo=_start+4 --defsym bar=_start -o %t.la64
# RUN: llvm-objdump -d %t.la32 | FileCheck %s --check-prefix=CHECK-32
# RUN: llvm-objdump -d %t.la64 | FileCheck %s --check-prefix=CHECK-64
# CHECK-32: 00 04 00 50   b       4
# CHECK-32: ff ff ff 57   bl      -4
# CHECK-64: 00 04 00 50   b       4
# CHECK-64: ff ff ff 57   bl      -4

# RUN: ld.lld %t.la32.o --defsym foo=_start+0x7fffffc --defsym bar=_start+4-0x8000000 -o %t.la32.limits
# RUN: ld.lld %t.la64.o --defsym foo=_start+0x7fffffc --defsym bar=_start+4-0x8000000 -o %t.la64.limits
# RUN: llvm-objdump -d %t.la32.limits | FileCheck --check-prefix=LIMITS-32 %s
# RUN: llvm-objdump -d %t.la64.limits | FileCheck --check-prefix=LIMITS-64 %s
# LIMITS-32:      ff fd ff 53   b       134217724
# LIMITS-32-NEXT: 00 02 00 54   bl      -134217728
# LIMITS-64:      ff fd ff 53   b       134217724
# LIMITS-64-NEXT: 00 02 00 54   bl      -134217728

# RUN: not ld.lld %t.la32.o --defsym foo=_start+0x8000000 --defsym bar=_start+4-0x8000004 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-RANGE %s
# RUN: not ld.lld %t.la64.o --defsym foo=_start+0x8000000 --defsym bar=_start+4-0x8000004 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-RANGE %s
# ERROR-RANGE: relocation R_LARCH_B26 out of range: 134217728 is not in [-134217728, 134217727]; references 'foo'
# ERROR-RANGE: relocation R_LARCH_B26 out of range: -134217732 is not in [-134217728, 134217727]; references 'bar'

# RUN: not ld.lld %t.la32.o --defsym foo=_start+1 --defsym bar=_start-1 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-ALIGN-1 %s
# RUN: not ld.lld %t.la64.o --defsym foo=_start+1 --defsym bar=_start-1 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-ALIGN-1 %s
# ERROR-ALIGN-1: improper alignment for relocation R_LARCH_B26: 0xFFFFFFFFFFFFFFFB is not aligned to 4 bytes

# RUN: not ld.lld %t.la32.o --defsym foo=_start+2 --defsym bar=_start-2 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-ALIGN-2 %s
# RUN: not ld.lld %t.la64.o --defsym foo=_start+2 --defsym bar=_start-2 -o /dev/null 2>&1 | FileCheck --check-prefix=ERROR-ALIGN-2 %s
# ERROR-ALIGN-2: improper alignment for relocation R_LARCH_B26: 0xFFFFFFFFFFFFFFFA is not aligned to 4 bytes

.global _start
_start:
     b foo
     bl bar
