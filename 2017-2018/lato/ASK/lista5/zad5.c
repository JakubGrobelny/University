#include <stdio.h>

/*
 * puzzle5:
 *  subq $24, %rsp	# rezerwowanie 24 bajtów na stosie
 *  movq %rsp, %rdi	# zapisywanie wskaźnika stosu w %rdi
 *  call readlong	# wzywanie procedury readlong
 *  leaq 8(%rsp), %rdi	# przesuwanie wskaźnika stosu o 8 (zapisanego w %rdi)
 *  call readlong	# wzywanie procedury readlong
 *  movq (%rsp), %rax	# zapisanie liczby ze szczytu stosu do %rax
 *  cqto		# konwersja quad na oct (%rax -> %rdx:%rax)
 *  idivq 8(%rsp)	# dzieli wartość w %rdx:%rax przez drugą liczbę na stosie
 *  xorl %eax, %eax	# czyszczenie %rax
 *  testq %rdx, %rdx	# ZF 1 jeśli zero
 *  sete %al		# ustawia bit w %al jeżeli ZF 1
 *  addq $24, %rsp	# usuwa ramkę stosu
 *  ret			# return
*/

long puzzle5() }

}
