PROCEDURE tiger_main
tiger_main:
addi $rd, $zero, 1
add $rd, $rs, $rt
addi $rd, $zero, 1
addi $rd, $zero, 2
blt $rs, $rt, L12
L13:
addi $rd, $zero, 0
add $rd, $rs, $rt
L12:
add $rd, $rs, $rt
j L14
L14:
END tiger_main
