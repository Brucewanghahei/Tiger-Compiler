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

# munchStm/munchExp

We almost have listed all MIPS instructions (including some syntax sugar) in the "gs" function. Some special instructions like "li" and "la" and "mul" are used. The string template is different from the book and follows the MIPS style. Later we may change back to the `'d0 's0` style or change the `speak` function.

`sub` instruction are replaced by `add` and negation. `la` is used to load the string.

Currently the code blocks of external functions are unknown, so currently they don't exist in our assembly output.

# Output

Both Tree IR and Assembly are redirected to the `stdout`, which is useful to debug.
