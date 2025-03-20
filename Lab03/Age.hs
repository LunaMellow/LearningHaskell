
-- Run:     stack runhaskell Age.hs
-- Test:    doctest Age.hs

-- | Version A: Strict Type Safety
--
-- >>> addAge (Age 25) (Age 30)
-- Age 55
--
-- >>> addAge (Age 10) (Age (-5))
-- Age *** Exception: Number cannot be negative
newtype Age = Age Int deriving (Show)
addAge :: Age -> Age -> Age
addAge (Age x) (Age y)
    | x < 0 || y < 0 = errorWithoutStackTrace "Number cannot be negative"
    | otherwise = Age (x + y)

-- | Version B: Flexible Type Safety (Cannot do negative check)
-- 
-- >>> addAgeB (AgeFlex 25) (AgeFlex 30)
-- AgeFlex 55
--
-- >>> addAgeB (AgeFlex 10) (AgeFlex 5)
-- AgeFlex 15
newtype AgeB = AgeFlex Int deriving (Show, Num)
addAgeB :: AgeB -> AgeB -> AgeB
addAgeB x y = x + y

-- | Get the integer value of an Age.
--
-- >>> ageVal (Age 25)
-- 25
--
-- >>> ageVal (Age 10)
-- 10
ageVal :: Age -> Int
ageVal (Age x) = x

-- | Adds two numbers of any numeric type.
--
-- Examples:
-- >>> addNumber 10 5
-- 15
--
-- >>> addNumber 3.5 2.5
-- 6.0
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