{- Sudoku solver program. Written by Marcel Goh on 22 July 2018 
 - The program:
 - 1. Asks for filename of a text file via standard input.
 -   (File must contain at least 81 numbers)
 - 2. Reads the first 81 numbers in the file and prints the puzzle grid.
 -   (The puzzle is assumed to be solvable)
 - 3. Solves the puzzle and prints the solution grid.
 -}

import Data.Char

-- returns the first 81 numbers found in string s as an int list
createGrid :: String -> [Int]
createGrid s = aux s 81 []
    where
      aux s n lst =
        case n of
          0 -> reverse lst
          _ -> case s of
                 c:cs -> if isDigit c
                            then aux cs (n - 1) (digitToInt c : lst)
                            else aux cs n lst

-- converts int list to string and adds newlines every 9 chars to
-- represent 1x81 list as a printable 9x9 grid
stringGrid :: [Int] -> String
stringGrid lst = addNewlines [] 0 $ reverse $ map intToDigit lst
    where
      -- helper function expects reversed input list
      addNewlines output counter input =
        case input of
          []   -> output
          c:cs -> if counter `mod` 9 == 0 && counter /= 0
                     then addNewlines (c : '\n' : output) (counter + 1) cs
                     else addNewlines (c : ' ' : output) (counter + 1) cs

-- fills blanks in problem grid with elements from solution list
fill :: [Int] -> [Int] -> [Int]
fill = aux []
    where
      aux :: [Int] -> [Int] -> [Int] -> [Int]
      aux acc problem solution =
        case problem of
          []   -> reverse acc
          n:ns -> if n == 0
                     then aux (head solution : acc) ns (tail solution)
                     else aux (n : acc) ns solution

-- returns row of grid at index
getRow :: Int -> [Int] -> [Int]
getRow index grid =
    map snd $ filter (\x -> fst x `div` 9 == index) $ zip [0..] grid

-- returns column of grid at index
getCol :: Int -> [Int] -> [Int]
getCol index grid =
    map snd $ filter (\x -> fst x `mod` 9 == index) $ zip [0..] grid

-- returns box of grid at index
getBox :: Int -> [Int] -> [Int]
getBox index grid =
    map snd $ filter (\x -> fst x `mod` 9 `div` 3 + fst x `div` 27 * 3 == index) $ zip [0..] grid

-- checks if putting num into cell is a safe operation and returns boolean
safe :: Int -> Int -> [Int] -> Bool
safe cell num grid =
    let row = cell `div` 9
        col = cell `mod` 9
        box = cell `mod` 9 `div` 3 + cell `div` 27 * 3
    in not (num `elem` getRow row grid ||
            num `elem` getCol col grid ||
            num `elem` getBox box grid)

-- recursive backtrack algorithm: returns a list of solutions to fill blanks in problem grid
solve :: [Int] -> [Int]
solve grid =
    let blanks = filter (0 ==) grid
        indicesOfBlanks = map fst $ filter (\x -> snd x == 0) $ zip [0..] grid
    in try indicesOfBlanks [] blanks []
       where
         try unfilled filled blanks solution =
           case blanks of
             []   -> reverse solution
             n:ns -> if n < 9
                        then let guess = n + 1
                                 ansSoFar = reverse solution ++ blanks
                                 cell = head unfilled
                             in if safe cell guess $ fill grid ansSoFar
                                   -- guess by pushing from unfilled to filled stacks and from 
                                   -- blank stack to solution stack
                                   then try (tail unfilled) (cell : filled) ns (guess : solution)
                                   else try unfilled filled (guess : ns) solution
                             -- backtrack: push from filled to unfilled and solution to blank
                        else try (head filled : unfilled) (tail filled) (head solution : 0 : ns) (tail solution)

-- asks user for input and prints to terminal
main = do putStrLn "Enter name of file to load:"
          filename <- getLine
          contents <- readFile filename
          let grid = createGrid contents
          putStrLn "Problem grid:"
          putStrLn $ stringGrid grid
          let solution = solve grid
          putStrLn "Solution grid:"
          putStrLn $ stringGrid $ fill grid solution
