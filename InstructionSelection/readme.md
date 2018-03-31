Instruction Selection
---

## T.CALL
First 4 parameters of a function call is saved in `a0` - `a3`(static link, the implicit first parameter is not counted). The rest is pushed into the stack(frame).

# Frame
## Registers
We add all registers that will be used in Mips in `mipsframe.sml` and implement a `map` to provide mappings `reg -> reg_name`.
## procEntryExit
In a function call, registers that needs to be used later by the caller should be saved. So an instruction that declares these registers as `src` is prepended to the function body in `procEntryExit2`.

As we don't know about register allocation in this phase, `procEntryExit3` is implemented as a scaffold.
