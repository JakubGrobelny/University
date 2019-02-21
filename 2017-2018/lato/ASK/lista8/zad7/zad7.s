.section .bss
.lcomm buf, 1

.section .text
.global _start
_start:
	stdin = 0
	stdout = 1
	bytes = 1
	end = 0
	movq $66, (buf)
	jmp WRITE
READ:
	movl $stdin, %ebx
	movl $buf, %ecx
	movl $bytes, %edx
	movl $3, %eax
	syscall

	# sprawdzanie, czy znak jest wielką/małą literą
	cmpq $end, (buf)
	je END
	cmpq $65, (buf)
	jl WRITE
	cmpq $90, (buf)
	jle TO_LOWER
	cmpq $97, (buf)
	jl WRITE
	cmpq $122, (buf)
	jg WRITE

	# to_upper
	subq $32, (buf)
	jmp WRITE

TO_LOWER:
	addq $32, (buf)

WRITE:
	movl $4, %eax
	movl $stdout, %ebx
	movl $buf, %ecx
	movl $bytes, %edx
	syscall

	jmp READ

END:
	#exit(0)
	movl $1, %eax
	movl $0, %ebx
	syscall
