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
data Antipode = Antipode Coord Coord Double
-- For sorting purposes we only care about the error
instance Eq Antipode where
    (Antipode c1 c2 e) == (Antipode d1 d2 f) = 0.01 > abs (e - f)
instance Ord Antipode where
    (Antipode c1 c2 e) `compare` (Antipode d1 d2 f) = e `compare` f

-- get a point's coordinates
coordFromPoint :: Point -> Coord
coordFromPoint (Point c _) = c

-- get a point's temperature
tempFromPoint :: Point -> Double
tempFromPoint (Point _ t) = t

-- print a point to standard out
printPoint :: Point -> IO ()
printPoint (Point (Coord lat long) temp) = do printf "(%.2f, %.2f, %.2f)\n" lat long temp

-- get coordinates of opposite side of the world
opposite :: Coord -> Coord
opposite (Coord lat long) =
    if long > 0 then Coord (-lat) (long - 180) else Coord (-lat) (long + 180)

-- find distance between two coordinates in km, assuming a spherical globe
-- uses the haversine formula
haversine :: Coord -> Coord -> Double
haversine (Coord lat1 long1) (Coord lat2 long2) =
    let phi1 = lat1 * (pi/180)
        phi2 = lat2 * (pi/180)
        dphi = (lat2-lat1) * (pi/180)
        dlambda = (long2-long1) * (pi/180)
        a = (sin (dphi/2)) * (sin (dphi/2)) + (cos phi1) * (cos phi2) * (sin (dlambda/2)) * (sin (dlambda/2))
        c = 2 * (atan2 (sqrt a) (sqrt (1 - a)))
    in 6371 * c

-- constructs an antipode from two coordinates
makeAntipode :: Coord -> Coord -> Antipode
makeAntipode c1 c2 = (Antipode c1 c2 (haversine (opposite c1) c2))

-- get the error from an antipode
getErr :: Antipode -> Double
getErr (Antipode _ _ err) = err

-- constructs a list of antipodes (with their error) from a list of coordinates
antipodeList :: [Coord] -> [Antipode]
antipodeList coordinates =
    let buildAcc c cs acc =
            case cs of
              []      -> acc
              car:cdr -> buildAcc c cdr ((makeAntipode c car):acc)
        iterate coords acc =
            case coords of
              []   -> acc
              c:cs -> iterate cs (buildAcc c cs acc)
    in iterate coordinates []

