#include <stdint.h>
#include <stdio.h>

/*
 * div:
 *	movl %edi, %edi # czyszczenie gornej polowy n
 *	salq $32,  %rsi # przesuwanie d do lewej polowy
 *	movl $32,  %edx # ustawianie iteratora na 32
 *	movl $0x80000000, %ecx	# maska z jedynką z przodu
 *	xorl %eax, %eax # czyszczenie %eax
 * .L3: addq %rdi, %rdi # dodawanie n do n
 *	movq %rdi, %r8  # zapisywanie wyniku w %r8
 *	subq %rsi, %r8  # odejmowanie d od %r8
 *	js   .L2	# skok do L2, jeżeli wynik był ujemny
 *	orl  %ecx, %eax # or maski i %eax
 *	movq %r8,  %rdi # zapisywanie %r8 w n
 * .L2: shrl %ecx	# przesunięcie maski o jeden w prawo
 *      decl %edx	# zmniejszenie iteratora o 1
 *	jne  .L3	# skok do L3, jeżeli iterator jest niezerowy
 *	ret		# return
 */

uint32_t div(uint32_t n, uint32_t d) {
    uint64_t n64 = n;
    uint64_t d64 = (uint64_t)d << 32;
    uint32_t mask = 0x80000000;
    uint32_t result = 0;
    uint64_t temp = 0;

    for (int i = 32; i > 0; i--) {
	n64 += n64;
	temp = n64;
	temp -= d64;

	uint64_t SF = temp & (1UL << 63);
	
	if (!SF) {
	    result |= mask;
	    n64 = temp;
	}

	mask >>= 1;
    }

    return result;	    
}

int main() {
    uint32_t a;
    uint32_t b;

    scanf("%d%d", &a, &b);
    printf("%d\n", (uint32_t)div(a, b));
    printf("%d\n", (uint32_t)puzzle3(a, b));
    return 0;
}


