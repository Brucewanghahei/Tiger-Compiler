Tests
---
We have created a test script (test.sh) to test all the testcases (test1.tig to test49.tig) for this type checking phase.

You may refer to `test.log` to see the test results

Features
---
1. The error output is optimized. Types (showing all types on the referring path) and symbols are shown in the error output.

2. Support recusive type/func declaration.

3. Break detection on different loop levels

4. Each variable (VarEntry) has a `assignable` tag.

5. The formals of FunEntry includes the original symbols
