extern printf
extern putchar

section .data
str_show_float0 db "%f", 0
float_main0 dd 3.14

section .text
global main

main:
    push rbp
    mov rbp, rsp
    sub rsp, 16

    ; trying to load the float in a rax and then into xmm0
    movss xmm0, [rel float_main0]

    call show_float
    mov dword [rbp-4], eax
    mov rax, 0
    jmp .L.function_end_main
.L.function_end_main:
    mov rsp, rbp
    pop rbp
    ret

global show_float
show_float:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov dword [rbp-4], edi
    mov rdi, str_show_float0
    mov esi, dword [rbp-4]
    call printf
    mov rdi, 10
    call putchar
    xor rax, rax
    jmp .L.function_end_show_float
.L.function_end_show_float:
    mov rsp, rbp
    pop rbp
    ret

