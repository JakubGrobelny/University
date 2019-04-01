    .globl cmp

    .text
cmp:
    xorq %rax,  %rax	#czyszczenie %r8
    cmpq %rdi, %rsi	#ustawia CF jezeli x >= y
    adcq $1, %rax	#0, jeżeli x < y, 1 jeżeli x >= y
    cmpq %rsi, %rdi	#ustawia CF jezeli y >= x
    adcq $-1, %rax
    
    subq %rsi, %rdi
    movq $1, %r8
    shl $63, %r8
    and %rdi, %r8
    shr $62, %r8
    neg %r8
    addq %r8, %rax

    ret


