-- Some algorithms using concepts from algebra
-- Last updated 11 Sep 2018 by Marcel Goh

-- calculates the gcd of a and b and returns a triple
-- (gcd(a, b), r, s) such that
-- gcd(a, b) = ra + sb
euclid :: Int -> Int -> (Int, Int, Int)
euclid a b =
  let extended :: Int -> Int -> (Int, Int) -> (Int, Int) -> (Int, Int, Int)
      extended a b s t =
        let q = a `div` b
            r = a `mod` b
            updatePair :: (Int, Int) -> Int -> (Int, Int)
            updatePair (f, s) q = (s, f - q * s)
        in if r == 0
           then (b, snd s, snd t)
           else extended b r (updatePair s q) (updatePair t q)
  in extended a b (1, 0) (0, 1)

-- takes a positive integer and returns the list of powers of two that sum to it
-- example: 457 = 2^8 + 2^7 + 2^6 + 2^3 + 2^0 so
-- twoPowers 457 returns [8,7,6,3,0]
twoPowers :: Int -> [Int]
twoPowers n =
  let buildList :: Int -> Int -> [Int] -> [Int]
      buildList n exp acc
        | q == 0    = if r == 0 then acc else exp : acc
        | otherwise = if r == 0
                      then buildList q (succ exp) acc
                      else buildList q (succ exp) (exp : acc)
        where q = n `div` 2
              r = n `mod` 2
  in buildList n 0 []
