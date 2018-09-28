-- Some functions involving groups and Cayley tables
-- Meant for loading into GHCi
-- Last updated on 26 September 2018 by Marcel Goh

import Data.List
import Text.Printf

data Element = Int Int | Char Char | String String
         deriving (Eq, Show)
-- a group is a set and a table corresponding to it
-- (we cannot define it as a set and operation because some operations are
-- easily definable in code)
data Group = Group [Element] [[Element]]

-- creates a group given a set of integers and a binary operation
groupFromOperation :: [Int] -> (Int -> Int -> Int) -> Group
groupFromOperation ints operation =
  let set = map Int ints
      table = map (\l -> map Int l) $ map (\g -> map (\x -> operation g x) ints) ints
  in Group set table


-- given a set and Cayley table (a list of rows), prints the table in a readable format
-- supports any kind of Element
printCayley :: Group -> IO ()
printCayley (Group set table) =
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

-- determines if given group is abelian
abelian :: Group -> Bool
abelian (Group _ rows) =
  let cols = getCols rows
      -- both tables must be of equal size (but all Cayley tables should be square anyway)
      tableEquals :: [[Element]] -> [[Element]] -> Bool
      tableEquals t1 t2 =
        case t1 of
          [] -> True
          _  -> if head t1 /= head t2 then False else tableEquals (tail t1) (tail t2)
  in tableEquals rows cols

-- checks if group contains identity element

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
quatGroup :: Group
quatGroup = Group quatSet quatTable

-- U(30) for testing
thirtyGroup :: Group
thirtyGroup = groupFromOperation [1,7,11,13,17,19,23,29] (\x y -> (x*y) `mod` 30)

-- dihedral groups D2, D3 for testing
e = Char 'e'
a = Char 'a'
b = Char 'b'
c = Char 'c'
d = Char 'd'
f = Char 'f'
-- D2
rectangleSet :: [Element]
rectangleSet = [e, a, b, c]
rectangleTable :: [[Element]]
rectangleTable = [[e,a,b,c],
                  [a,e,c,b],
                  [b,c,e,a],
                  [c,b,a,e]]
rectangleGroup :: Group
rectangleGroup = Group rectangleSet rectangleTable
-- D3
triangleSet :: [Element]
triangleSet = [e, a, b, c, d, f]
triangleTable :: [[Element]]
triangleTable = [[e,a,b,c,d,f],
                 [a,e,d,f,b,c],
                 [b,f,e,d,c,a],
                 [c,d,f,e,a,b],
                 [d,c,a,b,f,e],
                 [f,b,c,a,e,d]]
triangleGroup :: Group
triangleGroup = Group triangleSet triangleTable
