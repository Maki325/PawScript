print64:
  push rax
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
pop rax
ret

add64:
  push rbp
  mov rbp, rsp
  sub rsp, 8 ; 8 bytes is our int, because we aint bitches
  ; So we use `rsp + <variable offset>` for local variables
  ; Or we use `rbp - <variable offset>` for local variables

  mov rax, [rbp + 16]
  mov [rbp - 8], rax

  mov rax, [rbp + 24]
  add [rbp - 8], rax
  
  mov rax, [rbp - 8]
  mov rdi, [rbp + 32]
  mov [rdi], rax

  mov rsp, rbp
  pop rbp
ret

add64Rax:
  push rbp
  mov rbp, rsp
  sub rsp, 8 ; 8 bytes is our int, because we aint bitches
  ; So we use `rsp + <variable offset>` for paramaters variables
  ; Or we use `rbp - <variable offset>` for local variables

  mov rax, [rbp + 16] ; We start from 16 because 8 is the RBP we pushed (I think???)
  mov [rbp - 8], rax

  mov rax, [rbp + 24]
  add [rbp - 8], rax
  
  mov rax, [rbp - 8]

  mov rsp, rbp
  pop rbp
ret

global _start
_start:
  push rbp
  push rax
  mov rbp, rsp
  sub rsp, 8 ; Variable `c`
  lea rax, [rbp - 8] ; pointer to `c` variable, even though we use []
  mov [rax], DWORD 4

  push rbp
  mov rbp, rsp
  sub rsp, 24 ; 16 for input and 8 for output pointer
  mov [rbp - 8], rax
  mov rax, 89
  mov [rbp - 16], rax
  mov rax, 45
  mov [rbp - 24], rax

  call add64

  mov rsp, rbp
  pop rbp

  mov rdi, [rbp - 8]
  call print64


  push rbp
  mov rbp, rsp
  ; sub rsp, 16 ; 16 for input and 8 for output pointer
  ; mov rax, 45
  ; mov [rbp - 8], rax
  ; mov rax, 45
  ; mov [rbp - 16], rax

  push 45
  push 46

  call add64Rax

  mov rsp, rbp
  pop rbp

  mov rdi, rax
  call print64
  
  mov rax, 60
  mov rdi, 0
  syscall