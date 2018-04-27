.text
tig_main:
move $sp, $fp
addi $sp, $sp, -4
#sw $ra, 0($sp)
sw $ra, -4($fp)
addi $sp, $sp, -8
sw $fp, 0($sp)
la $a0, L2
#jal printf
addi $sp, $sp, 8
lw $ra, -4($fp)
#addi $sp, $sp, 4
move $sp, $fp
lw $fp, 0($sp)
jr $ra
.data
L2:
#.word 11
.asciiz "hello world"
