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
