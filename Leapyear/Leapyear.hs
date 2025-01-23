module Leapyear (main) where

isLeapYear :: Int -> Bool
isLeapYear year
    | divisible 400 == 0 = True
    | divisible 100 == 0 = False
    | divisible 4   == 0 = True
    | otherwise          = False
    where divisible n = year `mod` n

main :: IO ()
main = do
    putStrLn "\n-------------------------------------"
    putStrLn "\n(0) Quit program\n(1) Check leap year"
    quit <- getLine
    let quitInt = read quit :: Int
    if quitInt == 0
    then putStrLn "\nExiting program...\n"
    else do
        putStrLn "\nEnter year to check if it's a leap year:"
        yearInput <- getLine
        let year = read yearInput :: Int
        if isLeapYear year
            then putStrLn "\nIt's a leap year!" >> main
            else putStrLn "\nNot a leap year." >> main