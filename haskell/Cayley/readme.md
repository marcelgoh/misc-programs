# Cayley Table Analyzer

A program that determines the properties of groups by generating and analyzing Cayley tables.  

For now, the best way to use this is to load the `Main.hs` file into GHCi. Then to use any of the groups defined in `Data.hs`, prefix the group name with `Data.`. Example:
```
$ ghci
*Main> :load Main.hs
*Main> printCayley Data.thirtyGroup
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
```

## Authors
Code: Marcel Goh