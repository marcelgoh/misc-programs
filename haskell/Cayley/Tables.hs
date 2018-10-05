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

-- quaternion group for testing
o = String "1"
o' = String "-1"
i = String "i"
i' = String "-i"
j = String "j"
j' = String "-j"
k = String "k"
k' = String "-k"
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

-- U(30) for testing
thirtyGroup :: Group
thirtyGroup = groupFromOperation [1,7,11,13,17,19,23,29] (\x y -> (x*y) `mod` 30)

-- dihedral groups D2, D3 for testing
e = Char 'e'
a = Char 'a'
b = Char 'b'
c = Char 'c'
d = Char 'd'
f = Char 'f'
-- D2
rectangleSet :: [Element]
rectangleSet = [e, a, b, c]
rectangleTable :: [[Element]]
rectangleTable = [[e,a,b,c],
                  [a,e,c,b],
                  [b,c,e,a],
                  [c,b,a,e]]
rectangleGroup :: Group
rectangleGroup = Group rectangleSet rectangleTable
-- D3
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

-- addition without 0 (no identity) - for testing
noId :: Group
noId = groupFromOperation [1,2,3,4,5] (\x y -> x+y)
