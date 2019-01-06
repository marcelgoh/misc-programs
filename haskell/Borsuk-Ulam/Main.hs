-- Program to find two diametrically opposed points on the globe with 
-- the same temperature. Written by Marcel Goh.

import Text.Read

-- A point on the globe is defined as a latitude, longitude, and temperature.
data Point = Point Double Double Int

handle :: String -> IO ()
handle input =
    case readMaybe input :: Maybe Int of
      Just n -> case n of
                  1 -> do putStrLn "Option 1 chosen."
                          main
                  2 -> do putStrLn "Option 2 chosen."
                          main
                  3 -> do putStrLn "Closing program..."
                  _ -> do putStrLn "Not a valid option."
                          main
      Nothing -> do putStrLn "Not a valid option"
                    main

main :: IO ()
main = do
    putStrLn "****************************************"
    putStrLn "Choose an option by entering a number "
    putStrLn "1) Import new data"
    putStrLn "2) Find antipodes with same temperature"
    putStrLn "3) Exit the program"
    input <- getLine
    handle input
