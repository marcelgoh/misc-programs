module Analyze where

-- Some functions involving groups and Cayley tables
-- Last updated on 26 September 2018 by Marcel Goh

import Data.List
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

-- determines if group contains identity element
hasId :: Group -> Bool
hasId (Group set rows) =
  let cols = getCols rows
      -- checks through table and sees if one row/col matches set
      findMatch :: [Element] -> [[Element]] -> Bool
      findMatch set table =
        case table of
          []   -> False
          l:ls -> if set == l
                  then True
                  else findMatch set ls
  in (findMatch set rows) && (findMatch set cols)

