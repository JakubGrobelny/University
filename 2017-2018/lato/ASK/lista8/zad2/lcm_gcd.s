.section .text
.global lcm_gcd
.type lcm_gcd, @function
lcm_gcd:
	a = %rdi
	b = %rsi
	lcm = %rax
	gcd = %rdx
	
	cmpq a, b
	jnz START
	movq a, lcm
	movq b, gcd
	ret

START:
	temp  = %r10
	old_a = %r8
	old_b = %r9
	
	movq a, old_a
	movq b, old_b 
LOOP:
	# if b > a swap a and b
	cmpq a, b
	jle NO_SWAP

	#swapping
	movq a, temp
	movq b, a
	movq temp, b

NO_SWAP:
	
	# if b == 0 then end
	testq b, b
	jz END_GCD
	
	# a -= b
	subq b, a
	jmp LOOP	

END_GCD:
	
	# %r10 = gcd
	movq a, temp

	# %rax = a * b
	movq old_a, lcm
	mulq old_b

	# %rax = (a * b) / gcd
	divq temp

	# result.lcm = %rax
	# result.gdc = %r10
	movq temp, gcd
	
	ret

.size lcm_gcd, .-lcm_gcd
