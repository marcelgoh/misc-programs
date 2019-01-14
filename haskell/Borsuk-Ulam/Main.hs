-- Program to find two diametrically opposed points on the globe with 
-- the same temperature. Written by Marcel Goh.

import Data.Csv
import Data.Either
import Data.List
import Data.List.Split
import Network.Curl.Download
import Text.Printf
import Text.Read

import qualified Data.ByteString.Char8 as C

import Point

-- removes the first n lines from string
removeLines :: String -> Int -> String
removeLines str n =
    if n == 0
       then str
       else case str of
              c:cs -> if c == '\n'
                         then removeLines cs (n - 1)
                         else removeLines cs n

-- creates list of points, sorted by temp, from list of rows
getPoints :: [String] -> [Point]
getPoints strList =
    let getMaybe :: String -> Maybe Point
        getMaybe str =
            let fields = splitOn "," str
            in case fields of
                 (_:_:_:s1:s2:s3:rest) -> if s1 /= "" && s2 /= "" && s3 /= ""
                                        then Just (Point (Coord (read s1 :: Double)
                                                                (read s2 :: Double))
                                                         (read s3 :: Double))
                                        else Nothing
                 _                -> Nothing
        iterate :: [String] -> [Point] -> [Point]
        iterate strs acc =
            case strs of
              []   -> acc
              s:ss -> case getMaybe s of
                        Just p  -> iterate ss (p:acc)
                        _       -> iterate ss acc
    in sort (iterate strList [])

-- restructures list to group coordinates with same temperature
-- assumes point list is already sorted by temperature
byTemp :: [Point] -> [([Coord], Double)]
byTemp [] = []
byTemp points =
    let iterate pts acc (list, temp) =
            case pts of
              []             -> (list, temp):acc
              (Point c t):ps -> if 0.01 > t - temp
                                   then iterate ps acc (c:list, temp)
                                   else iterate ps ((list, temp):acc) (c:[], t)
    in iterate points [] ([], tempFromPoint (head points))

-- converts coordinate list to antipode lists
toAntipode :: [([Coord], Double)] -> [([Antipode], Double)]
toAntipode tuples =
        -- remove temperatures with only one location
    let trimSingles :: [([Coord], Double)] -> [([Coord], Double)] -> [([Coord], Double)]
        trimSingles tups acc =
            case tups of
              [] -> acc
              ((list, t):rest) -> case list of
                                    c:[] -> trimSingles rest acc
                                    _    -> trimSingles rest ((list, t):acc)
        convert :: ([Coord], Double) -> ([Antipode], Double)
        convert (cs, d) = (antipodeList cs, d)
    in map convert (trimSingles tuples [])

minAntipode :: [([Antipode], Double)] -> [(Antipode, Double)]
minAntipode tuples =
    map (\(as, d) -> (minimum as, d)) tuples

leastError :: [(Antipode, Double)] -> (Antipode, Double)
leastError [] = error "Cannot call on empty list: LEAST_ERROR"
leastError tuples =
    foldr (\ (a1, d1) (a2, d2) -> if a1 < a2 then (a1, d1) else (a2, d2)) (head tuples) tuples

printByTemp :: (Antipode, Double) -> IO ()
printByTemp (a, t) =
    do printf "{%.1f deg. C:\n" t
       printAntipode a
       putStrLn "}"

-- handle different user inputs
handle :: String -> IO ()
handle input =
    case readMaybe input :: Maybe Int of
      Just n -> case n of
                  1 -> do putStrLn "Trying to find data from web..."
                          doc <- openURI "https://aviationweather.gov/adds/dataserver_current/current/metars.cache.csv"
                          case doc of
                            (Left _)        -> putStrLn "Error writing file."
                            (Right bytestr) -> do writeFile "./Current/data.csv" (C.unpack bytestr)
                                                  putStrLn "Data fetched successfully from server."
                          main
                  2 -> do putStrLn "Analysing data on file..."
                          doc <- readFile "./Current/data.csv"
                          let ants = toAntipode $ byTemp $ getPoints $ lines (removeLines doc 6)
                          printByTemp $ leastError $ minAntipode ants
                          main
                  3 -> do putStrLn "Enter the relative path of a file"
                          filename <- getLine
                          doc <- readFile filename
                          let ants = toAntipode $ byTemp $ getPoints $ lines (removeLines doc 6)
                          printByTemp $ leastError $ minAntipode ants
                          main
                  4 -> do putStrLn "Closing program..."
                  _ -> do putStrLn "Not a valid option."
                          main
      Nothing -> do putStrLn "Not a valid option"
                    main

-- main loop
main :: IO ()
main = do putStrLn "****************************************"
          putStrLn "Choose an option by entering a number "
          putStrLn "1) Update to current weather data"
          putStrLn "2) Find antipodes in current weather data"
          putStrLn "3) Import data from file"
          putStrLn "4) Exit the program"
          input <- getLine
          handle input
