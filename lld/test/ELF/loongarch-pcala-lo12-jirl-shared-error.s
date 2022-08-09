# REQUIRES: loongarch

# RUN: llvm-mc --filetype=obj --triple=loongarch32-unknown-elf %s -o %t.la32.o
# RUN: llvm-mc --filetype=obj --triple=loongarch64-unknown-elf %s -o %t.la64.o

# RUN: not ld.lld %t.la32.o -shared -o %t.la32.so 2>&1 | FileCheck %s
# RUN: not ld.lld %t.la64.o -shared -o %t.la64.so 2>&1 | FileCheck %s

.global foo
foo:
    pcalau12i   $t0, %pc_hi20(bar)
    ld.w        $t0, $t0, %pc_lo12(bar)

# CHECK: error: relocation R_LARCH_PCALA_LO12 cannot be used against symbol 'bar'; recompile with -fPIC
