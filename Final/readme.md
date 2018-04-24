Register Allocation
===

Control Flow
---
```
                    |---->|<-future--|<-future--|                      |-->|
                    |     +          |          |                      |   +
color --> build --> simplify --> coalesce --> freeze --> potential --> select --> actual (regAlloc) -->
 ^                        ^_________________________________|                              |
 |___________________________________(in the future)_______________________________________|
```

Our register allocation is implemented just as the graph above, each component is just a function.
The last expression of every function is just a function call which might diverge with condition,
connecting another component, like an edge in the graph.

## Notes
- These modules have not been implemented: `coalesce`, `freeze`, `actual spill`

- Since coalesce have not been implemented, we merge `freeze` into `simplify`,
which means that instead remove move edges one by one in `freeze`, we ignore move edges and try to simplify all nodes in `simplify`.

Assembly Output
---
We optimized assembly instructions' printing. Now it can output formatted assembly strings (before register allocation)

Reg-Alloc about caller/callee save registers
---
In the future, consider distinguishing caller save and callee save registers. Because the interference graph is not global (each frame has its own interference graph)

Output of register allocation
---
We also output the register allocation table. Each table contains a default mapping as the table below shows.
```
=========================
Register Allocation
t100($v1) <- $v1
t101($a0) <- $a0
t102($a1) <- $a1
t103($a2) <- $a2
t104($a3) <- $a3
t105($t0) <- $t0
t106($t1) <- $t1
t107($t2) <- $t2
t108($t3) <- $t3
t109($t4) <- $t4
t110($t5) <- $t5
t111($t6) <- $t6
t112($t7) <- $t7
t113($s0) <- $s0
t114($s1) <- $s1
t115($s2) <- $s2
t116($s3) <- $s3
t117($s4) <- $s4
t118($s5) <- $s5
t119($s6) <- $s6
t120($s7) <- $s7
t121($t8) <- $t8
t122($t9) <- $t9
t123($zero) <- $zero
t125($fp) <- $fp
t126($sp) <- $sp
t127($ra) <- $ra
t128($v0) <- $v0
```
