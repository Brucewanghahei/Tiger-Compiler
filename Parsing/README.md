Parsing
===

All grammers are based on the appendix of the tiger book.

About some Absyn builder
---

At the begining of the .grm file are some builer functions. The function 'buildLvalueVar' is a recursive function to construct a complex lvalue.

About precendence
---

Below is the list of precendence. We defined two abstract precendence symbol 'LOWESTPREC' and 'HIGHESTPREC' for convenience.

```
%nonassoc LOWESTPREC
%nonassoc DO OF THEN ASSIGN FUNCTION TYPE
%nonassoc IF
%nonassoc ELSE
%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS
%nonassoc HIGHESTPREC
```
