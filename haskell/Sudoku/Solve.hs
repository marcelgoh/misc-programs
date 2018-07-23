{- Sudoku solver program. WIP. Started by Marcel Goh on 22 July 2018 -}

import Data.Char

-- returns the first 81 numbers found in string s as an int list
createGrid :: String -> [Int]
createGrid s = aux s 81 []
    where
      aux s n lst =
        case n of
          -- corrects reversed list before returning
          0 -> reverse lst
          _ -> case s of
                 c:cs -> if isDigit c
                            -- constructs list in reverse order
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
                     -- constructs output string in reverse (i.e. correct) order
                     then addNewlines (c : '\n' : output) (counter + 1) cs
                     else addNewlines (c : ' ' : output) (counter + 1) cs

blankIndices :: [Int] -> [Int]
blankIndices = aux 0 []
    where
      aux counter indices grid =
        case grid of
          []     -> reverse indices
          n:ns -> if n == 0
                     then aux (counter + 1) (counter : indices) ns
                     else aux (counter + 1) indices ns

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

-- checks if putting num into cell is a safe operation
safe :: Int -> Int -> [Int] -> Bool
safe cell num grid =
    let row = cell `div` 9
        col = cell `mod` 9
        box = cell `mod` 9 `div` 3 + cell `div` 27 * 3
    in not (num `elem` getRow row grid ||
            num `elem` getCol col grid ||
            num `elem` getBox box grid)

   {-
solve :: [Int] -> [Int]
solve grid =
    let blanks = filter (0 ==) grid
        indices = blankIndices grid
    in try indices [] blanks []
       where
         try unfilled filled blanks solution =
           case blanks of
             []   -> reverse solution
             n:ns -> if n < 9
                        then let guess = 
                    -}

main = do putStrLn "Enter name of file to load: "
          filename <- getLine
          contents <- readFile filename
          let grid = createGrid contents
          putStrLn "Problem grid: "
          putStrLn $ stringGrid grid
          print $ show $ fill [1, 0, 3, 0, 0] [9, 9, 9]
          print $ show $ blankIndices [1, 0, 3, 0, 0]
          print $ show $ blankIndices [1, 1, 1]
          print $ show $ getRow 2 grid
          print $ show $ getCol 7 grid
          print $ show $ getBox 4 grid
          print $ show $ safe 4 4 grid
          print $ show $ safe 13 3 grid
