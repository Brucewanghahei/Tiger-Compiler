.text
tig_main:
la $a0, L2
jal printf
jal tig_print
jal tig_exit
.data
L2:
.word 11
.asciiz "hello world"
