.text
.globl __main
.globl __malloc

  la t0,__main
  sw t0,main,x3
  la t0,__malloc
  sw t0,malloc,x3
  la t0,heapstart
  sw t0,heapptr,x3
  lw t1,heapsize
  add t2,t0,t1
  sw t2,heapend,x3
  jal ra,__main
  jal ra,__halt
__main:
  addi sp,sp,-8
  sw fp,4(sp)
  sw ra,0(sp)
  addi fp,sp,4
  addi sp,sp,0
main__entry:
  addi a3,zero,1
  addi a1,zero,0
  addi a2,zero,5
  jal zero,label1
label1:
  addi t1,zero,0
  blt t1,a2,label2
  jal zero,label3
label2:
  add a1,a1,a3
  addi t1,zero,1
  sub a0,a2,t1
  addi a3,a1,0
  addi a1,a3,0
  addi a2,a0,0
  jal zero,label1
label3:
main__exit:
  addi sp,fp,-4
  lw fp,4(sp)
  lw ra,0(sp)
  addi sp,sp,8
  jalr zero,ra,0
__malloc:
  lw t0,heapptr
  lw t2,heapend
  add t1,t0,a0
  blt t2,t1,__eom
  sw t1,heapptr,x3
  addi a0,t0,0
  jalr zero,ra,0
__eom:
  xor a0,a0,a0
  jalr zero,ra,0
__halt:


.data

main:
  .word 0

malloc:
  .word 0

heapptr:
  .word 0

heapsize:
  .word 4194304

heapend:
  .word 0

heapstart:
  .word 0

