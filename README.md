# Advent of Code 2018 in Standard ML

Solutions tested with MosML. Run with eg.

```
cat 1.input | mosml 1-1.sml
```

For more speed, remove all `load` lines from the code and compile with mlton:

```
grep -vE 'load "' 6-1.sml > 6-1-mlton.sml
mlton 6-1-mlton.sml
cat 6.input | ./6-1-mlton
```
