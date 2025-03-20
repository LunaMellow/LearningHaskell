
-- Test: doctest ZipWithFibonacci.hs

-- | Fibonacci sequence using zipWith
--
-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]
--
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)