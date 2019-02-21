	.globl puzzle3

	.text
puzzle3:
	movl %edi, %edi
	salq $32, %rsi
	movl $32, %edx
	movl $0x80000000, %ecx
	xorl %eax, %eax
.L3:	addq %rdi, %rdi
	movq %rdi, %r8
	subq %rsi, %r8
	js .L2
	orl %ecx, %eax
	movq %r8, %rdi
.L2:	shrl %ecx
	decl %edx
	jne .L3
	ret
