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
It first copies the flow graph, then insert live-in and live-out sets to the nodes' attributes. Finally it iteratively update the live-in and live-out sets. 

Interference Graph
---
Firstly it registers all temps in "def" and "use" of each node. Then it iterate the { live-in } set and { live-in } set (a self join) to find the interference pair for each node. Finally, it collects the move information.


Output
---
We print the flow-graph, live-graph and interference-graph at the end of compiling. The function call instruction may have a large { use } set. Due to this, the interference graph may have a strongly connected graph on MIPS reserved registers.

