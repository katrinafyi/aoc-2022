# aoc-2022

the code files are inside the aoc directory. 
all of these take input from stdin. 
my puzzle inputs are given in this directory. 

python is run by invoking the submodules from the root of the repository:
```bash
python -m aoc.d1 < ./1.txt
```

haskell solutions should be run from the aoc subdirectory:
```bash
runghc ./d7.hs < ../7.txt
```
alternatively, you can compile with `ghc` for an order-of-magnitude speedup.
(if on Arch with `ghc` from the repositories, you'll need to specify -dynamic if missing module errors are thrown.) (-iaoc can be added to run from the repository root.)


