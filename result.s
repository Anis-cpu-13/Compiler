.text
.globl main
main:
  addi $sp, $sp, -12
  sw $ra, 8($sp)
  sw $fp, 4($sp)
  addi $fp, $sp, 8
  li $v0, 1312
  sw $v0, -8($fp)
ret0:
  addi $sp, $sp, 12
  lw $ra, 0($fp)
  lw $fp, -4($fp)
  jr $ra
%add:
  lw $t0, 0($sp)
  lw $t1, 4($sp)
  add $v0, $t0, $t1
  jr $ra
%sub:
  lw $t0, 0($sp)
  lw $t1, 4($sp)
  sub $v0, $t1, $t0
  jr $ra
%mul:
  lw $t0, 0($sp)
  lw $t1, 4($sp)
  mul $v0, $t0, $t1
  jr $ra
%div:
  lw $t0, 0($sp)
  lw $t1, 4($sp)
  div $v0, $t1, $t0
  jr $ra
%mod:
  lw $t0, 0($sp)
  lw $t1, 4($sp)
  div $v0, $t1, $t0
  mfhi $v0
  jr $ra
%puti:
  lw $a0, 0($sp)
  li $v0, 1
  syscall
  jr $ra
%putnl:
  la $a0, nl
  li $v0, 4
  syscall
  jr $ra
%geti:
  lw $a0, 0($sp)
  li $v0, 5
  syscall
  jr $ra
%puts:
  move $a0, $v0
  li $v0, 4
  syscall
  jr $ra
%putb:
  move $a0, $v0
  li $v0, 1
  syscall
  jr $ra
%gt:
  lw $t0, 4($sp)
  lw $t1, 0($sp)
  sgt  $v0, $t0, $t1
  jr $ra
%gte:
  lw $t0, 4($sp)
  lw $t1, 0($sp)
  sge  $v0, $t0, $t1
  jr $ra
%lt:
  lw $t0, 4($sp)
  lw $t1, 0($sp)
  slt  $v0, $t0, $t1
  jr $ra
%lte:
  lw $t0, 4($sp)
  lw $t1, 0($sp)
  sle  $v0, $t0, $t1
  jr $ra
%eq:
  lw $t0, 4($sp)
  lw $t1, 0($sp)
  seq  $v0, $t0, $t1
  jr $ra
%neq:
  lw $t0, 4($sp)
  lw $t1, 0($sp)
  sne  $v0, $t0, $t1
  jr $ra
%and:
  lw $t0, 4($sp)
  lw $t1, 0($sp)
  and  $v0, $t0, $t1
  jr $ra
%or:
  lw $t0, 4($sp)
  lw $t1, 0($sp)
  or   $v0, $t0, $t1
  jr $ra
%Xor:
  lw $t0, 4($sp)
  lw $t1, 0($sp)
  or   $v0, $t0, $t1
  jr $ra

.data
nl: .asciiz "\n" 
