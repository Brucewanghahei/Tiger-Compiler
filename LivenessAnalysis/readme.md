Liveness Analysis
===

Flow Graph
---
Flow graph is composed of nodes with type 
```
{def: TSet.set, use: TSet.set, move: (Temp.temp * Temp.temp) option}
```

where `move` stores the `dst` and `src` of a move edge.

Every node in the graph is an instruction. The `def` and `use` set of each node is built by rules below:

- a register will be added to `use` if it appears before its first definition in the current block(divided by labels). A definition set is maintained for each block, which will be clear when entering a new block.
- a register will be added to `def` if it is not used in current instruction.

P.S. Since the function `tiger_main` will be called through an instruction inserted by `procEntryExit2` at the end, the last node of the graph always has a `use` set of `[ZERO, RA, SP] @ calleesaveRegs`.

Live Graph
---

Interference Graph
---
