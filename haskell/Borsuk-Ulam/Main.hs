-- Program to find two diametrically opposed points on the globe with 
-- the same temperature. Written by Marcel Goh.

import Data.Either
import Network.Curl.Download
import Text.Read

import qualified Data.ByteString.Char8 as C

-- A point on the globe is defined as a latitude, longitude, and temperature.
data Point = Point Double Double Double

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
                  2 -> do putStrLn "Option 2 chosen."
                          main
                  3 -> do putStrLn "Closing program..."
                  _ -> do putStrLn "Not a valid option."
                          main
      Nothing -> do putStrLn "Not a valid option"
                    main

main :: IO ()
main = do putStrLn "****************************************"
          putStrLn "Choose an option by entering a number "
          putStrLn "1) Update to current weather data"
          putStrLn "2) Find antipodes with same temperature"
          putStrLn "3) Exit the program"
          input <- getLine
          handle input
