section .text
global egg_count
egg_count:
    mov rax, 0
.loop:
    cmp rdi, 0
    je .end
    mov rdx, rdi
    dec rdx,
    and rdi, rdx
    inc rax
    jmp .loop
.end:
    ret

%ifidn __OUTPUT_FORMAT__,elf64
section .note.GNU-stack noalloc noexec nowrite progbits
%endif
