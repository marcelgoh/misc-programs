{- Various sort algorithms. Written by Marcel Goh on 23 July 2018 -}

-- actually a "pseudo-quicksort" because not in-place
quicksort :: [Int] -> [Int]
quicksort []     = []
quicksort (n:ns) = (quicksort $ filter (n >) ns) ++ (n : (quicksort $ filter (n <=) ns))

-- mergesort
mergesort :: [Int] -> [Int]
mergesort =
    let merge :: [Int] -> [Int] -> [Int]
        merge ms []         = ms
        merge [] ns         = ns
        merge (m:ms) (n:ns) = if m <= n
                                 then m : (merge ms (n:ns))
                                 else n : (merge ns (m:ms))
        sort :: [Int] -> [Int]
        sort []  = []
        sort [n] = [n]
        sort ns =
            let fsthalf = take (length ns `div` 2) ns
                sndhalf = drop (length ns `div` 2) ns
            in merge (sort fsthalf) (sort sndhalf)
    in sort

main = do
    putStrLn "Enter numbers separated by spaces:"
    numString <- getLine
    let numList = map read $ words numString
    print $ show $ quicksort numList
    print $ show $ mergesort numList
