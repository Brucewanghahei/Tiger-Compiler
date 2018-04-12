Liveness Analysis
===

Flow Graph
---

Live Graph
---
It first copies the flow graph, then insert live-in and live-out sets to the nodes' attributes. Finally it iteratively update the live-in and live-out sets. 

Interference Graph
---
Firstly it registers all temps in "def" and "use" of each node. Then it iterate the { live-in } set and { live-in } set (a self join) to find the interference pair for each node. Finally, it collects the move information.


Output
---
We print the flow-graph, live-graph and interference-graph at the end of compiling. The function call instruction may have a large { use } set. Due to this, the interference graph may have a strongly connected graph on MIPS reserved registers.

