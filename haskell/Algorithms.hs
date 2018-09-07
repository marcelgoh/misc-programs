{- Some algorithms. Meant for loading into GHCi.
 - Last updated 24 July 2018
 -}

euclid :: Int -> Int -> Int
euclid a b
   | b == 0    = a
   | otherwise = euclid b (a `mod` b)
