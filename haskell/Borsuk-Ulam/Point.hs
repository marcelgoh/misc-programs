module Point where

import Text.Printf

-- A point on the globe is defined as a latitude, longitude, and temperature.
data Point = Point Double Double Double
-- For sorting purposes we only care about the temperature
instance Eq Point where
    (Point lat1 long1 temp1) == (Point lat2 long2 temp2) = 0.01 > abs (temp1 - temp2)
instance Ord Point where
    (Point lat1 long1 temp1) `compare` (Point lat2 long2 temp2) = temp1 `compare` temp2

-- A pair of points consists of two points and an error denoting how 
-- close they are to being antipodes.
data Antipode = Antipode Point Point Double
-- For sorting purposes we only care about the error
instance Eq Antipode where
    (Antipode p1 p2 e) == (Antipode q1 q2 f) = 0.01 > abs (e - f)
instance Ord Antipode where
    (Antipode p1 p2 e) `compare` (Antipode q1 q2 f) = e `compare` f

-- print a point to standard out
printPoint :: Point -> IO ()
printPoint p =
    case p of
      (Point d1 d2 d3) -> do printf "(%.4f, %.4f, %.4f)\n" d1 d2 d3

-- get coordinates of opposite side of the world
opposite :: Point -> (Double, Double)
opposite (Point lat long _) =
    if long > 0 then (-lat, long - 180) else (-lat, long + 180)

