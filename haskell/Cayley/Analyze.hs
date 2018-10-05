module Analyze where

-- Some functions involving groups and Cayley tables
-- Last updated on 5 October 2018 by Marcel Goh

import Data.List
import Data.Maybe
import Text.Printf

import Tables

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

-- returns identity of group or Nothing if no identity exists
getId :: Group -> Maybe Element
getId (Group set rows) =
  let cols = getCols rows
      -- checks if any element in set has the set itself as its row/col
      tryElem :: [Element] -> [[Element]] -> [[Element]] -> Maybe Element
      tryElem setLeft rows cols =
        case setLeft of
          []   -> Nothing
          e:es -> if (((head rows) == set) && (head cols) == set)
                  then Just e
                  else tryElem es (tail rows) (tail cols)
  in tryElem set rows cols

-- checks if group has an identity element
hasId :: Group -> Bool
hasId g = isJust $ getId g


-- applies the binary operation (a `dot` b) to a and b in Group
dot :: Element -> Element -> Group -> Maybe Element
dot a b (Group set rows) =
      -- returns the row corresponding to a in table
  let findRow :: Element -> [Element] -> [[Element]] -> Maybe [Element]
      findRow a setLeft rowsLeft =
        case setLeft of
          []   -> Nothing
          e:es -> if e == a then Just (head rowsLeft) else findRow a es (tail rowsLeft)
      aRow = findRow a set rows
      -- returns the Element corresponding to b in a's row
      findElem :: Element -> [Element] -> [Element] -> Maybe Element
      findElem b setLeft rowLeft =
        case setLeft of
          []   -> Nothing
          e:es -> if e == b then Just (head rowLeft) else findElem b es (tail rowLeft)
  in case aRow of
       Nothing    -> Nothing
       (Just row) -> findElem b set row

{- TODO
-- given an Element and a Group, find the inverse of that Element in the Group
getInv :: Element -> Group -> Maybe Element
getInv elem (Group set rows) =
-}
