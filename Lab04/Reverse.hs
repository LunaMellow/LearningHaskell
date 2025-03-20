
-- Run: stack runhaskell Reverse.hs

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

main :: IO ()
main = print (mreverse [1,2,3])