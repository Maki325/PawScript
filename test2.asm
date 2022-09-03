BITS 64
section .text
print64:
  mov   r9, -3689348814741910323
  sub   rsp, 40
  mov   BYTE [rsp+31], 10
  lea   rcx, [rsp+30]
.print64L2:
  mov   rax, rdi
  lea   r8, [rsp+32]
  mul   r9
  mov   rax, rdi
  sub   r8, rcx
  shr   rdx, 3
  lea   rsi, [rdx+rdx*4]
  add   rsi, rsi
  sub   rax, rsi
  add   eax, 48
  mov   BYTE [rcx], al
  mov   rax, rdi
  mov   rdi, rdx
  mov   rdx, rcx
  sub   rcx, 1
  cmp   rax, 9
  ja  .print64L2
  lea   rax, [rsp+32]
  mov   edi, 1
  sub   rdx, rax
  xor   eax, eax
  lea   rsi, [rsp+32+rdx]
  mov   rdx, r8
  mov   rax, 1
  syscall
  add   rsp, 40
ret
main:
  push rbp
  mov rbp, rsp
  sub rsp, 32

  mov rax, 1
  mov [rbp - 32], rax

  mov al, [rbp - 32]
  mov [rbp - 16], al

  mov rax, 78
  mov [rbp - 8], rax

  mov rdi, [rbp - 32]
  call print64
  mov rdi, [rbp - 24]
  call print64
  mov rdi, [rbp - 16]
  call print64
  mov rdi, [rbp - 8]
  call print64

  mov rax, 0

  mov rsp, rbp
  pop rbp
ret

global _start
_start:
  call main
  mov rdi, rax
  mov rax, 60
  syscall
