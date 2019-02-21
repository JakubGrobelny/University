	.globl cmp

	.text
cmp:
	xorq %rax, %rax
	cmpq %rdi, %rsi
	adcq $0x0, %rax
	subq %rsi, %rdi
	sbbq $0x0, %rax	

	ret


