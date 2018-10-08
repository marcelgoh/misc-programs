module Tables where

{- Contains a bunch of different groups to use in the Analyze program -}

-- data types for Elements and Groups
data Element = Int Int | Char Char | String String
         deriving (Eq, Show)
-- a group is a set and a table corresponding to it
-- (we cannot define it as a set and operation because some operations are not
-- easily definable in code)
data Group = Group [Element] [[Element]]

-- creates a group given a set of integers and a binary operation
groupFromOperation :: [Int] -> (Int -> Int -> Int) -> Group
groupFromOperation ints operation =
  let set = map Int ints
      table = map (\l -> map Int l) $ map (\g -> map (\x -> operation g x) ints) ints
  in Group set table

-- FOR TESTING --
e = Char 'e'
a = Char 'a'
b = Char 'b'
c = Char 'c'
d = Char 'd'
f = Char 'f'

o = String "1"
o' = String "-1"
i = String "i"
i' = String "-i"
j = String "j"
j' = String "-j"
k = String "k"
k' = String "-k"

-- quaternion group
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
-- generates Z_n
makeCyclic :: Int -> Group
makeCyclic n = groupFromOperation [0..n-1] (\x y -> (x+y) `mod` n)
-- examples of Z_n for convenience
z8 :: Group
z8 = makeCyclic 8
z17 :: Group
z17 = makeCyclic 17
-- generates U(n)
groupOfUnits :: Int -> Group
groupOfUnits n = groupFromOperation [x | x <- [0..n-1], gcd x n == 1] (\x y -> (x*y) `mod` n)
-- examples of U(n) for convenience
thirtyGroup :: Group
thirtyGroup = groupFromOperation [1,7,11,13,17,19,23,29] (\x y -> (x*y) `mod` 30)
sixtyGroup :: Group
sixtyGroup = groupFromOperation [1,7,11,13,17,19,23,29,31,37,41,43,47,49,53,59] (\x y -> (x*y) `mod` 60)
-- dihedral group D2
rectangleSet :: [Element]
rectangleSet = [e, a, b, c]
rectangleTable :: [[Element]]
rectangleTable = [[e,a,b,c],
                  [a,e,c,b],
                  [b,c,e,a],
                  [c,b,a,e]]
rectangleGroup :: Group
rectangleGroup = Group rectangleSet rectangleTable
-- dihedral group D3
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
-- jumbled up
jumbled :: Group
jumbled = Group [a,b,c,d,e] [[e,d,b,a,c],
                             [d,c,e,b,a],
                             [b,e,a,c,d],
                             [a,b,c,d,e],
                             [c,a,d,e,b]]

-- NOT GROUPS 
-- addition without 0 (no identity)
noId :: Group
noId = groupFromOperation [1,2,3,4,5] (\x y -> x+y)
-- multiplication with 0 (no inverses)
noInv :: Group
noInv = groupFromOperation [0,1,2,3,4] (\x y -> x*y)
-- repeated elements
reps :: Group
reps = Group [a,b,c,d] [[a,b,c,d],
                        [b,c,a,a],
                        [c,d,a,b],
                        [d,a,b,c]]
-- not closed
notClosed :: Group
notClosed = Group [a,b,c] [[a,b,c],
                           [b,d,a],
                           [c,a,b]]
-- not associative
notAssoc :: Group
notAssoc = Group [a,b,c,d,e] [[a,b,c,b,d],
                              [b,c,a,e,c],
                              [c,a,b,b,a],
                              [b,e,b,e,d],
                              [d,b,a,d,c]]
