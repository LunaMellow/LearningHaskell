module Main (main) where

main :: IO ()
main = mulTable 10

-- | mreverse is my own implementation of list reversal
--
-- >>> mreverse "Hello"
-- "olleH"
--
-- >>> mreverse [1,2,3]
-- [3,2,1]
--
mreverse :: [a] -> [a]
mreverse [] = [] -- Empty list remains empty
mreverse (x:xs) = mreverse xs ++ [x]

-- | Multiplies table of size `n x n` and prints it with padding
--
-- >>> mulTable 5
-- " 1  2  3  4  5"
-- " 2  4  6  8 10"
-- " 3  6  9 12 15"
-- " 4  8 12 16 20"
-- " 5 10 15 20 25"
--
mulTable :: Int -> IO ()
mulTable n = mapM_ putStrLn [unwords [pad (i * j) | j <- [1..n]] | i <- [1..n]]
    where
        pad :: Int -> String
        pad x = let str = show x in replicate (2 - length str) ' ' ++ str 
        -- Only pads up to 3 indexes. Which means that 100 will not be padded unless you do 3 - length str


