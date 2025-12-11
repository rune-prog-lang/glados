section .data
str_main0 db "Hello",10, 0
float_main0 dd 3.14
section .text
global main
main:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    ; int <- 3
    mov dword [rbp-12], 3
    ; str <- "Hello"
    mov rax, str_main0
    mov qword [rbp-8], rax
    ; float <- 3.14
    mov rax, float_main0
    mov rax, 
    ; WARNING: Unsupported IRASSIGN operand: IRConstFloat 3.14
    mov dword [rbp-16], 0
    xor rax, rax
    jmp .L.function_end_main
.L.function_end_main:
    mov rsp, rbp
    pop rbp
    ret

