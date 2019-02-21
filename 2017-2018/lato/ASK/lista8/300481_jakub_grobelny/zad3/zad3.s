.section .text
.global insert_sort
.type insert_sort, @function
insert_sort:
	size  = %r8
	first = %rdi
	last  = %rsi

	# obliczanie rozmiaru tablicy
	movq last, size
	subq first, size
	shrq $3, size
	
	i = %r9
	j = %r10
	element = %r11
	test = %r12

	movq $1, i
	
	jmp OUTER_TEST

# dla i = 1, dopóki i < size, i++ :
OUTER_LOOP:

	# element = array[i]
	movq (first, i, 8), element
	# j = i - 1
	movq i, j
	subq $1, j

# dopóki j >= 0 && array[j] > element:
INNER_LOOP:
	# j >= 0
	cmpq $0, j
	jl END_INNER
	# array[j] > element
	cmpq (first, j, 8), element
	jge END_INNER
	
	# zamiana j+1 elementu na j
	temp = %rdx
	movq (first, j, 8), temp
	movq temp, 8(first, j, 8)
	# j--
	decq j
	jmp INNER_LOOP	

END_INNER:
	# i++
	incq i
	# j++
	addq $1, j
	# array[j] = element
	movq element, (first, j, 8)

OUTER_TEST:
	# i < size
	cmpq i, size
	jg OUTER_LOOP

END:
	ret

.size insert_sort, .-insert_sort
