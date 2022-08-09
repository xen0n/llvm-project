# REQUIRES: loongarch

    .text
    .globl  _start
_start:
    pcalau12i   $t0, %pc_hi20(foo)
    ld.w        $t0, $t0, %pc_lo12(foo)
    move        $a0, $t0
    li.w        $a7, 93
    syscall     0

    .data
foo:
    .word       42
