.global main
.global _main

.text
main:
call _main
    pushq %rbp
    movq %rsp, %rbp
    call _main
    movq %rax, %rdi
    movq $60, %rax
    syscall

_main:
pushq %rbp
movq %rsp, %rbp
subq $16, %rsp
# Register allocation begins
# Register allocation: fromList [(0,PhysReg %rax),(1,PhysReg %rcx),(2,PhysReg %rdx),(3,PhysReg %r10),(4,PhysReg %r11),(5,PhysReg %rbx),(6,PhysReg %r12)]
movq $105, %rax
movq $1056, %rcx
movq %rcx, %rdx
movq %rax, %rax
cqto
idivq %rcx
movq %r10, %r10
movq %r10, %rcx
movq %rdx, %rax
movq %rcx, %rdx
movq %rax, %rax
cqto
idivq %rcx
movq %r10, %r11
movq %r11, %rcx
movq %rdx, %rax
movq %rcx, %rdx
movq %rax, %rax
cqto
idivq %rcx
movq %r10, %rbx
movq %rbx, %rcx
movq %rdx, %rax
movq %rcx, %rdx
movq %rax, %rax
cqto
idivq %rcx
movq %r10, %r12
movq %r12, %rcx
movq %rdx, %rax
movq %rax, %rax
# Register allocation ends
movq %rbp, %rsp
popq %rbp
ret

