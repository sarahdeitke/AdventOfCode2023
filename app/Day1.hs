module Day1 where
import System.IO
import Data.List (isPrefixOf)
import Data.Char (isDigit, digitToInt)

day1Funky :: IO ()
day1Funky = do
    calibrationDoc <- readFile "app/Day1Input.txt"
    let result = sum (map computeCalibration (lines calibrationDoc))
    putStrLn ("Total result: " ++ show result)

-- input - string calibration code
-- output - first and last digit from string joined
computeCalibration :: String -> Int
computeCalibration myString =
    let digits = convertStringsToDigits myString in

    let headDigit = head digits in
    let lastDigit = last digits in
    (10 * (digitToInt headDigit)) + (digitToInt lastDigit)


convertStringsToDigits :: String -> String
convertStringsToDigits = go ""
  where
    go :: String -> String -> String
    go acc [] = acc
    go acc (x:xs)
        | isDigit x = go (acc ++ [x]) xs
        | "one"   `isPrefixOf` (x:xs) = go (acc ++ "1") (drop 1 xs)
        | "two"   `isPrefixOf` (x:xs) = go (acc ++ "2") (drop 1 xs)
        | "three" `isPrefixOf` (x:xs) = go (acc ++ "3") (drop 1 xs)
        | "four"  `isPrefixOf` (x:xs) = go (acc ++ "4") (drop 1 xs)
        | "five"  `isPrefixOf` (x:xs) = go (acc ++ "5") (drop 1 xs)
        | "six"   `isPrefixOf` (x:xs) = go (acc ++ "6") (drop 1 xs)
        | "seven" `isPrefixOf` (x:xs) = go (acc ++ "7") (drop 1 xs)
        | "eight" `isPrefixOf` (x:xs) = go (acc ++ "8") (drop 1 xs)
        | "nine"  `isPrefixOf` (x:xs) = go (acc ++ "9") (drop 1 xs)
        | otherwise = go acc xs
