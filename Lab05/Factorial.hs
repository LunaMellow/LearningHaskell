
-- Test: doctest Factorial.hs

-- | Computes the factorial of a non-negative integer.
-- 
-- The factorial of a number n is the product of all positive integers less than or equal to n.
-- 
-- >>> factorial 0
-- 1
--
-- >>> factorial 1
-- 1
--
-- >>> factorial 2
-- 2
--
-- >>> factorial 3
-- 6
--
-- >>> factorial 4
-- 24
--
-- >>> factorial 5
-- 120
--
-- >>> factorial 6
-- 720
--
-- >>> factorial 7
-- 5040
--
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)