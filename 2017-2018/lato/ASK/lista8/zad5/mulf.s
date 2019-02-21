.section .text
.global mulf
.type mulf, @function
mulf:
	a = %edi
	b = %esi
	
	sign = %r8d
	man  = %r9d
	exp  = %r10d
	temp = %r11d

	sign_mask = 0x80000000
	man_mask  = 0x007FFFFF
	man_one   = 0x00800000
	exp_mask  = 0x7F800000
	
	movl a, sign
	xorl b, sign
	andl $sign_mask, sign

	movl a, exp
	andl $exp_mask, exp
	movl b, temp
	andl $exp_mask, temp
	addl temp, exp
	subl $127, exp
	andl $exp_mask, exp

	movl a, man
	andl $man_mask, man
	orl  $man_one, man
	movl b, temp
	andl $man_mask, temp
	orl  $man_one, temp
	imul temp, man
	shr $23, man	
	movl $man_mask, temp
	orl  $man_one,  temp
	andl temp, man

	movl $man_one, temp
	shrl $1, temp
	cmpl temp, man
	jl END
	
	shll $1, man
	subl $1, exp

END:
	movl man, %eax
	orl exp, %eax
	orl sign, %eax
	ret

.size mulf, .-mulf
