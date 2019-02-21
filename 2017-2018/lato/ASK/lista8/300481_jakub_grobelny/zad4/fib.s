.section .text
.global fibonacci
.type fibonacci, @function
fibonacci:
	# n == 0 || n == 1
	cmpq $1, %rdi
	jnle REC
	movq %rdi, %rax
	ret
REC:
	# zapisywanie 'n' na stosie
	pushq %rdi
	# wywoływanie funkcji dla n - 1
	subq  $1, %rdi
	call fibonacci
	# zdejmowanie zapisanego 'n' ze stosu
	popq %rdi
	# zapisywanie wyniku wywołania dla n - 1 na stosie
	pushq %rax
	# wywoływanie funkcji dla n - 2
	subq $2, %rdi
	call fibonacci
	# zdejmowanie wyniku dla n - 1 ze stosu
	popq %r8
	# zwracanie sumy wyników wywołań dla n - 1 i n - 2
	addq %r8, %rax
	ret	
.size fibonacci, .-fibonacci
