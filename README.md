# Monoidal Parallel Parsing

This repository contains several monoidal parsing programs, building up in complexity to a verifier for a subset of XML. To compile them, make sure `mpl` points to a [MaPLe](https://github.com/MPLLang/mpl) compiler and then run one of
```bash
mpl cellularAutomaton.mlb
mpl longestWord.mlb
mpl longestQuote.mlb
mpl dyck.mlb
mpl xml.mlb
mpl xmlLong.mlb
```

To test speedup for xmlLong, use `test.sh`.
