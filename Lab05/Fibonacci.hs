
-- Test: doctest Fibonacci.hs

-- Fibonacci sequence
--
-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]
--
fibs :: [Integer]
fibs = 0 : 1 : next fibs
  where
    next (a : t@(b:_)) = (a + b) : next t

-- N-th Fibonacci number
--
-- >>> fib2 5
-- 5
--
-- >>> fib2 10
-- 55
--
-- >>> fib2 1
-- 1
--
-- >>> fib2 2
-- 1
--
fib2 :: Int -> Integer
fib2 n = fibs !! n
