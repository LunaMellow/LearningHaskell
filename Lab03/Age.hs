
-- Run: stack runhaskell Age.hs

-- Version A: Strict Type Safety
newtype Age = Age Int deriving (Show)
addAge :: Age -> Age -> Age
addAge (Age x) (Age y)
    | x < 0 || y < 0 = error "Number cannot be negative"
    | otherwise = Age (x + y)

-- Version B: Flexible Type Safety (Cannot do negative check)
newtype AgeB = AgeFlex Int deriving (Show, Num)
addAgeB :: AgeB -> AgeB -> AgeB
addAgeB x y = x + y

-- Get age value int
ageVal :: Age -> Int
ageVal (Age x) = x

-- AddNumber
addNumber :: Num a => a -> a -> a
addNumber x y = x + y

-- Main
main :: IO ()
main = do
    putStrLn "Hi, what is your name?"
    nameInput <- getLine
    putStrLn "and what is your age?"
    ageInput <- getLine

    let age = Age (read ageInput :: Int)
    putStrLn $ "Hello " ++ nameInput ++ ", in ten years, you will be " ++ show (ageVal (addAge age (Age 10)))