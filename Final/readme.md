Final Phase
===

## Patches for translate:
1. When unCxing an Ex, we take false label if the expression is zero and true label if it is non-zero (not just if it is equal to 1)
2. Field variable is added, which was treated as subscript variable before
3. Safety checks are added when indexing by a subscript by multiplying the wordSize
4. Array size is added and stored at the head of the array when it is created, which makes it possible to do bound check for safety
5. When making a new level, we set all the newest argument as escapes
6. For string comparison, since the externalCall only contains “stringEqual”, we will not consider about “stringLT” and “stringGT”

## Patches for semantic:
1. Built-in functions such as print, chr, ord are added into the base environment, which were missed in the previous phase.
