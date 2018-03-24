Translating to IR
---

This is the fancy part of the compiler ... I mean you can do a lot of fancy work here (and later phases) if you have time

Test
---
You can run "Main.compile <filename>" to get the result. For this phase, you will see the type checking errors (if exists) and the IR tree.

For example:
```
val it = true : bool
 = SEQ(
  LABEL tiger_main,
  MOVE(
   TEMP t101,
   ESEQ(
    EXP(
     CONST 0),
    CONST 1)))
 val it = [()] : unit list
```

We also implement a test.sh to automate the test.

What we implement
---

# Function & View shift
## procEntryExit
As the number of variables that should be stored in the frame/registers cannot be known until `reg alloc` phase,
we only put the label and the body of the function in the `fragment` for it. So only procEntryExit1 is implemented in this phase.
`procEntryExit2` and `procEntryExit3` which handle the stack/frame pointer and the callee save/restore of `view shift` will be implemented later.
