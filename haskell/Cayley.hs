-- Some functions involving groups and Cayley tables
-- Meant for loading into GHCi
-- Last updated on 26 September 2018 by Marcel Goh

import Data.List
import Text.Printf

data Element = Int Int| Char Char | String String
         deriving (Eq, Show)

-- creates a table of Elements given a set of integers and a binary operation
-- only supports Ints
fillCayley :: [Int] -> (Int -> Int -> Int) -> [[Element]]
fillCayley set operation =
  map (\l -> map Int l) $ map (\g -> map (\x -> operation g x) set) set

-- given a set and Cayley table (a list of rows), prints the table in a readable format
-- supports any kind of Element
printCayley :: [Element] -> [[Element]] -> IO ()
printCayley set table =
  let display :: Element -> String
      display elem =
        case elem of
          (String s) -> id s
          (Char c)   -> printf "%c" c
          (Int i)    -> show i
      setStr = intercalate " " (map (\l -> printf "%3s" (display l)) set)
      tableStr = map (\list -> intercalate " " (map (\l -> printf "%3s" (display l)) list)) table
      iterate set tableStr =
        case set of
          []           -> return ()
          (elem:elems) -> do printf "%3s" (display elem)
                             putStr " | "
                             putStrLn (head tableStr)
                             iterate elems (tail tableStr)
      -- draws two hyphens per number
      drawLine len =
        if len > 0
        then do putStr "----"
                drawLine (pred len)
        else do putChar '\n'
  in do putStr "  * | "
        putStrLn setStr
        putStr "------"
        drawLine (length set)
        iterate set tableStr

-- given a list of rows, returns a list of columns
getCols :: [[Element]] -> [[Element]]
getCols rows =
  let iter acc rowsLeft =
        case rowsLeft of
          []:_ -> reverse acc
          _    -> iter ((map head rowsLeft) : acc) (map tail rowsLeft)
  in iter [] rows

-- quaternion group for testing
o = String "1"
o' = String "-1"
i = String "i"
i' = String "-i"
j = String "j"
j' = String "-j"
k = String "k"
k' = String "-k"
quatSet :: [Element]
quatSet = [o, o', i, i', j, j', k, k'];
quatTable :: [[Element]]
quatTable = [[o, o', i, i', j, j', k, k'],
             [o', o, i', i, j', j, k', k],
             [i, i', o', o, k, k', j', j],
             [i', i, o, o', k', k, j, j'],
             [j, j', k', k, o', o, i, i'],
             [j', j, k, k', o, o', i', i],
             [k, k', j, j', i', i, o', o],
             [k', k, j', j, i, i', o, o']]

-- U(30) for testing
thirtySet :: [Element]
thirtySet = map Int [1, 7, 11, 13, 17, 19, 23, 29]
thirtyTable :: [[Element]]
thirtyTable = fillCayley [1,7,11,13,17,19,23,29] (\x y -> (x*y) `mod` 30)

-- dihedral group D3 for testing
e = Char 'e'
a = Char 'a'
b = Char 'b'
c = Char 'c'
d = Char 'd'
f = Char 'f'
triangleSet :: [Element]
triangleSet = [e, a, b, c, d, f]
triangleTable :: [[Element]]
triangleTable = [[e,a,b,c,d,f],
                 [a,e,d,f,b,c],
                 [b,f,e,d,c,a],
                 [c,d,f,e,a,b],
                 [d,c,a,b,f,e],
                 [f,b,c,a,e,d]]
