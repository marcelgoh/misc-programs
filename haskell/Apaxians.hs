{- Kattis: apaxiaaans -}

compact :: String -> String
compact str =
  let iter :: Char -> String -> String -> String
      iter lastLetter acc strLeft =
        case strLeft of
          []     -> reverse acc
          (c:cs) -> if c == lastLetter
                    then iter lastLetter acc cs
                    else iter c (c:acc) cs
  in case str of
    []   -> []
    c:cs -> iter c [c] cs

main = do str <- getLine
          putStrLn $ compact str
