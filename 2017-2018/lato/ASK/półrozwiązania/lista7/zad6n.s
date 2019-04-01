.section .data

#.globl baz
baz:
    .ascii "abc"
    .zero 1
    .long 42
    .quad -3
    .long 1068827777
    .zero 4

 .section .text   

.globl getptr
getptr:
    movq baz, %rax
    ret
