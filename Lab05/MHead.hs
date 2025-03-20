
-- Test: doctest MHead.hs

-- | Returns the first element of a list using pattern matching.
--
-- >>> mhead1 [1,2,3]
-- 1
--
-- >>> mhead1 ["apple", "banana", "cherry"]
-- "apple"
--
-- >>> mhead1 []
-- *** Exception: The list is empty
--
mhead1 :: [a] -> a
mhead1 [] = errorWithoutStackTrace "The list is empty"
mhead1 (x:xs) = x

-- | Returns the first element of a list using function guards.
--
-- >>> mhead2 [1,2,3]
-- 1
--
-- >>> mhead2 ["apple", "banana", "cherry"]
-- "apple"
--
-- >>> mhead2 []
-- *** Exception: The list is empty
--
mhead2 :: [a] -> a
mhead2 xs
    | null xs = errorWithoutStackTrace "The list is empty" 
    | otherwise = case xs of 
        (x:_) -> x

-- | Returns the first element of a list using if-else expressions.
-- If the list is empty, it throws an error.
--
-- >>> mhead3 [1,2,3]
-- 1
-- >>> mhead3 ["apple", "banana", "cherry"]
-- "apple"
-- >>> mhead3 []
-- *** Exception: The list is empty
--
mhead3 :: [a] -> a
mhead3 xs = if null xs then errorWithoutStackTrace "The list is empty" else let (x:_) = xs in x

-- | Returns the first element of a list using pattern matching with a let binding.
-- Throws an error if the list is empty.
--
-- >>> mhead4 [1,2,3]
-- 1
-- >>> mhead4 ["apple", "banana", "cherry"]
-- "apple"
-- >>> mhead4 []
-- *** Exception: The list is empty
--
mhead4 :: [a] -> a
mhead4 xs = case xs of
    (x:_) -> x
    [] -> errorWithoutStackTrace "The list is empty"

-- | Returns the first element of a list using a where binding.
-- If the list is empty, it throws an error.
--
-- >>> mhead5 [1,2,3]
-- 1
-- >>> mhead5 ["apple", "banana", "cherry"]
-- "apple"
-- >>> mhead5 []
-- *** Exception: The list is empty
--
mhead5 :: [a] -> a
mhead5 xs = header 
    where 
        header = case xs of 
            (x:_) -> x
            [] -> errorWithoutStackTrace "The list is empty"

-- | Returns the first element of a list using a case expression.
-- Throws an error if the list is empty.
--
-- >>> mhead6 [1,2,3]
-- 1
-- >>> mhead6 ["apple", "banana", "cherry"]
-- "apple"
-- >>> mhead6 []
-- *** Exception: The list is empty
--
mhead6 :: [a] -> a
mhead6 xs = case xs of
    (x:_) -> x
    [] -> errorWithoutStackTrace "The list is empty"
