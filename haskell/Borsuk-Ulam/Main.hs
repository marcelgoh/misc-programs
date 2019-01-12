-- Program to find two diametrically opposed points on the globe with 
-- the same temperature. Written by Marcel Goh.

import Data.Csv
import Data.Either
import Data.List
import Data.List.Split
import Network.Curl.Download
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
                                        then Just (Point (read s1 :: Double)
                                                         (read s2 :: Double)
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
                          let points = getPoints (lines (removeLines doc 6))
                          mapM_ printPoint points
                          main
                  3 -> do putStrLn "Closing program..."
                  _ -> do putStrLn "Not a valid option."
                          main
      Nothing -> do putStrLn "Not a valid option"
                    main

-- main loop
main :: IO ()
main = do putStrLn "****************************************"
          putStrLn "Choose an option by entering a number "
          putStrLn "1) Update to current weather data"
          putStrLn "2) Find antipodes with same temperature"
          putStrLn "3) Exit the program"
          input <- getLine
          handle input
