# Sudoku Solver

My first Haskell program: a sudoku solver! I wrote [my original solution](https://github.com/marcelgoh/misc-programs/blob/master/c/sudoku_solver.c) in C a month before. Porting it to Haskell was not as easy as I thought it would be! The program reads input from a `.txt` file, taking the first 81 numbers in the file as the puzzle to solve. Will not work correctly if puzzle is not solvable. Some puzzles and solutions are included for testing purposes.  

Usage:  
```
ghc Solve.hs
./Solve
<FILENAME, e.g. Puzzles/s1.txt>
```

## Authors
Code: Marcel Goh  
Puzzles and solutions by Timo Mantere and Janne Koljonen of the University of Vaasa. [More puzzles can be found at their webpage](http://lipas.uwasa.fi/~timan/sudoku/).