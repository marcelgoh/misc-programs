module Point where

import Text.Printf

-- A coordinate on the globe is a latitude and longitude
data Coord = Coord Double Double

-- A point on the globe is defined as a coordinate location and a temperature.
data Point = Point Coord Double
-- For sorting purposes we only care about the temperature
instance Eq Point where
    (Point c1 temp1) == (Point c2 temp2) = 0.01 > abs (temp1 - temp2)
instance Ord Point where
    (Point c1 temp1) `compare` (Point c2 temp2) = temp1 `compare` temp2

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
      (Point (Coord lat long) temp) -> do printf "(%.4f, %.4f, %.4f)\n" lat long temp

-- get coordinates of opposite side of the world
opposite :: Coord -> Coord
opposite (Coord lat long) =
    if long > 0 then Coord (-lat) (long - 180) else Coord (-lat) (long + 180)

-- find distance between two coordinates in km, assuming a spherical globe
-- uses the haversine formula
dist :: Coord -> Coord -> Double
dist (Coord lat1 long1) (Coord lat2 long2) =
    let phi1 = lat1 * (pi/180)
        phi2 = lat2 * (pi/180)
        dphi = (lat2-lat1) * (pi/180)
        dlambda = (long2-long1) * (pi/180)
        a = (sin (dphi/2)) * (sin (dphi/2)) + (cos phi1) * (cos phi2) * (sin (dlambda/2)) * (sin (dlambda/2))
        c = 2 * (atan2 (sqrt a) (sqrt (1 - a)))
    in 6371 * c

here :: Coord
here = (Coord 32.9697 (-96.80322))
there :: Coord
there = (Coord 29.46786 (-98.53506))
