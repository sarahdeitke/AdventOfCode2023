module Day2 where
import System.IO
import Data.List.Split
import Data.List (intercalate)
import Data.Bool

day2Funky :: IO()
day2Funky = do
    gameDoc <- readFile "app/Day2Input.txt"
    let result = sum (map computeValidGame (lines gameDoc))
    putStrLn ("Total result: " ++ show result)

-- input is game row
-- output is game # if game is valid, otherwise 0
computeValidGame :: String -> Int
computeValidGame game =
    let removePrefix = drop 5 game in -- pulling out "Game "
    let gameNumberSplit = splitOn ": " removePrefix in

    let gameNumber = read (head gameNumberSplit) :: Int in
    let gameSetsStr = last gameNumberSplit in

    -- Part 2
    let gameSets = replaceSubstring ";" "," gameSetsStr in
    let setsArr = splitOn ", " gameSets in
    computePowerGame setsArr 0 0 0

    -- Part 1
    -- let gameSets = splitOn ";" gameSetsStr in
    -- let validGameArr = map computeValidSet gameSets in

    -- let isGameInvalid = elem False validGameArr in

    -- if (isGameInvalid) then 0
    -- else gameNumber

computePowerGame :: [String] -> Int -> Int -> Int -> Int
computePowerGame [] maxR maxG maxB = maxR*maxG*maxB
computePowerGame (h:t) maxR maxG maxB =
    let valArr = splitOn " " h in
    let diceNum = read (head valArr) :: Int in
    let diceColor = last valArr in
    
    if (diceColor == "red" && diceNum > maxR) then computePowerGame t diceNum maxG maxB
    else if (diceColor == "green" && diceNum > maxG) then computePowerGame t maxR diceNum maxB
    else if (diceColor == "blue" && diceNum > maxB) then computePowerGame t maxR maxG diceNum
    else computePowerGame t maxR maxG maxB


-- input is 2 red, 2 green
computeValidSet :: String -> Bool
computeValidSet set =
    let colorDice = splitOn "," set in
    
    let setValidArr = map computeValidDice colorDice in

    let isSetInvalid = elem False setValidArr in

    not isSetInvalid

-- input is  2 green
computeValidDice :: String -> Bool
computeValidDice die = 
    let dropPrefix = drop 1 die in -- " 2 green" -> "2 green"
    let dieVals = splitOn " " dropPrefix in

    let color = last dieVals in
    let numDie = read (head dieVals) :: Int in
    
    if (color == "red" && numDie > 12) then False
    else if (color == "green" && numDie > 13) then False
    else if (color == "blue" && numDie > 14) then False
    else True

-- Replace occurrences of a substring in a string
replaceSubstring :: String -> String -> String -> String
replaceSubstring old new str =
    intercalate new $ splitOn old str