# compiler
command
```
cabal run compiler-exe -- test/Compiler/Frontend/Language/Tiger/samples/queens.tig
gcc runtime.s test/Compiler/Frontend/Language/Tiger/samples/queens.tig.s -o queens
./queens
```
