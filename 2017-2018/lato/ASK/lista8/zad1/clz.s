.section .text
.global clz
.type clz, @function
clz:
	number = %rdi # liczba podana na wejściu
	bit    = %rcx  # bit, który jest kandydatem na najbardziej wysunięty na prawo
	bit_s  = %cl
	mask   = %r8  # maska składająca się z samych jedynek na prawo od wartości 'bit'
	full   = 0xFFFFFFFFFFFFFFFF
	b      = %r9 # początek przedziału
	e      = %r10 # koniec przedziału
	temp   = %r11
	offset = %rdx

	cmp $0, number
	jl RET_ZERO
	jne START
	movq $64, %rax
	ret

	movq $1, mask
	shlq $32, mask
	cmp mask, number
	ja START
	movq $1, offset

START:
	# początkowe wartości w rejestrach
	movq $32, bit
	movq $0,  b
	movq $63, e
	movq $full, mask
	shrq bit_s, mask

	jmp TEST

LOOP:
	
	cmpq b, e
	je END
	
	# wyznaczanie środka przedziału
	movq e, bit
	addq b, bit
	shrq $1, bit

	# wyznaczanie nowej maski
	movq $full, mask
	shrq bit_s, mask

TEST:
	
	# b == e - 1 ?
	movq e, temp
	subq $1, temp
	cmpq b, temp
	jne NOT_EQUAL
	addq $1, b
	movq b, bit
	shrq $1, mask

NOT_EQUAL:
	# Porównywanie maski z liczbą. Jeżeli liczba jest większa, to należy
	# szukać rozwiązania na lewo od aktualnego bitu. 
	cmpq mask, number
	ja SEARCH_LEFT
	# Jeżeli są równe, to zwracamy aktualny bit
	je END
	# Jeżeli liczba jest mniejsza, to sprawdzamy aktualny bit liczby, jeżeli
	# jest 1, to zwracamy aktualny bit, w przeciwnym razie szukamy wyniku po prawej stronie
	movq $1, mask
	movq bit, %rax	
	movq $63, bit
	subq %rax, bit
	shlq bit_s, mask
	andq number, mask
	movq %rax, bit
	jnz END
	jmp SEARCH_RIGHT

SEARCH_LEFT:
	movq bit, e
	jmp LOOP

SEARCH_RIGHT:
	movq bit, b
	jmp LOOP

END:
	movq bit, %rax
	addq offset, %rax
	ret
RET_ZERO:
	movq $0, %rax
	ret

.size clz, .-clz
.end
