.intel_syntax noprefix
.global main
.global _main
.text
main:
	call _main
	mov rdi, rax
	mov rax, 0x3C
	syscall
_main:
	push rbp
	mov rbp, rsp
	sub rsp, 8
	mov rbx, 2147483646
	mov rcx, 2
	mov rdx, 3
	mov rsi, rcx
	imul rsi, rdx
	mov rdi, 3
	mov r8, rsi
	imul r8, rdi
	mov r9, 11
	mov r10, r8
	imul r10, r9
	mov r11, 31
	mov r12, r10
	imul r12, r11
	mov r13, 151
	mov r14, r12
	imul r14, r13
	mov r15, 331
	mov QWORD PTR [rbp-8], r14
	push rax
	mov rax, QWORD PTR [rbp-8]
	imul rax, r15
	mov QWORD PTR [rbp-8], rax
	pop rax
	push rdx
	mov rax, rbx
	cdq
	idiv QWORD PTR [rbp-8]
	mov rbx, rax
	pop rdx
	mov rax, rbx
	mov rsp, rbp
	pop rbp
	ret
