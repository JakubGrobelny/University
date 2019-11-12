_JB_RBX = 0
_JB_RBP = 1 # start of stack 
_JB_R12 = 2
_JB_R13 = 3
_JB_R14 = 4
_JB_R15 = 5
_JB_RSP = 6 # current location in stack
_JB_RIP = 7 # instruction pointer 

        .text

        .globl Setjmp
        .type Setjmp,@function
Setjmp: # int Setjmp(Jmpbuf env : rdi)
	movq    (%rsp),%r11
	movq    %rbx,(_JB_RBX * 8)(%rdi)
	movq    %rbp,(_JB_RBP * 8)(%rdi)
	movq    %r12,(_JB_R12 * 8)(%rdi)
	movq    %r13,(_JB_R13 * 8)(%rdi)
	movq    %r14,(_JB_R14 * 8)(%rdi)
	movq    %r15,(_JB_R15 * 8)(%rdi)
	movq    %rsp,(_JB_RSP * 8)(%rdi)
	movq    %r11,(_JB_RIP * 8)(%rdi)
	xorl	%eax,%eax
	ret
        .size Setjmp, . - Setjmp
        
        .globl Longjmp
        .type Longjmp,@function
Longjmp: # noreturn void Longjmp(Jmpbuf env : rdi, int val : esi)
	movq    (_JB_RBX * 8)(%rdi),%rbx
	movq    (_JB_RBP * 8)(%rdi),%rbp
	movq    (_JB_R12 * 8)(%rdi),%r12
	movq    (_JB_R13 * 8)(%rdi),%r13
	movq    (_JB_R14 * 8)(%rdi),%r14
	movq    (_JB_R15 * 8)(%rdi),%r15
	movq    (_JB_RSP * 8)(%rdi),%rsp
	movq    (_JB_RIP * 8)(%rdi),%r11
	movl	%esi,%eax
	testl	%eax,%eax
	jnz	1f  # if (!eax) eax = 1; else goto 1;
	incl	%eax
1:	movq	%r11,(%rsp) # put old %rip on the stack
	ret
        .size Longjmp, . - Longjmp
