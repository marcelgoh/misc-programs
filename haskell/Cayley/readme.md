# Cayley Table Analyzer

A program that determines the properties of groups by generating and analyzing Cayley tables.  

For now, the best way to use this is to load the `Main.hs` file into GHCi.
```
$ ghci
*Main> :load Main.hs
```

## Features
Print out Cayley tables for your favourite groups, like the quaternion group!
```
*Main> printCayley quatGroup
  * |   1  -1   i  -i   j  -j   k  -k
--------------------------------------
  1 |   1  -1   i  -i   j  -j   k  -k
 -1 |  -1   1  -i   i  -j   j  -k   k
  i |   i  -i  -1   1   k  -k  -j   j
 -i |  -i   i   1  -1  -k   k   j  -j
  j |   j  -j  -k   k  -1   1   i  -i
 -j |  -j   j   k  -k   1  -1  -i   i
  k |   k  -k   j  -j  -i   i  -1   1
 -k |  -k   k  -j   j   i  -i   1  -1
```
Find the identity element in a group in a snap!
```
*Main> printCayley jumbled
  * |   a   b   c   d   e
--------------------------
  a |   e   d   b   a   c
  b |   d   c   e   b   a
  c |   b   e   a   c   d
  d |   a   b   c   d   e
  e |   c   a   d   e   b
*Main> isGroup jumbled
True
*Main> getId jumbled
Just (Char 'd')
```
Get the inverse without a worry!
```
*Main> printCayley thirtyGroup
  * |   1   7  11  13  17  19  23  29
--------------------------------------
  1 |   1   7  11  13  17  19  23  29
  7 |   7  19  17   1  29  13  11  23
 11 |  11  17   1  23   7  29  13  19
 13 |  13   1  23  19  11   7  29  17
 17 |  17  29   7  11  19  23   1  13
 19 |  19  13  29   7  23   1  17  11
 23 |  23  11  13  29   1  17  19   7
 29 |  29  23  19  17  13  11   7   1
*Main> getInv (Int 17) thirtyGroup
Just (Int 23)
```
Checks for associativity and commutativity work out of the box as well!

## Authors
Code: Marcel Goh
