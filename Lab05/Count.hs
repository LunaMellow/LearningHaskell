
-- Test: doctest Count.hs

-- | Count occurrences of an element in a list.
--
-- >>> count 10 [2,10,3,10,4]
-- 2
--
-- >>> count 'a' "banana"
-- 3
--
-- >>> count 5 [1,2,3,4,5,5,5,6]
-- 3
--
-- >>> count True [False, True, True, False, True]
-- 3
--
count :: Eq a => a -> [a] -> Int
count x = sum . zipWith (\a b -> if a == b then 1 else 0) (repeat x)
--
-- More intuitive approach:
-- count x = sum . map (\a -> if a == x then 1 else 0)

