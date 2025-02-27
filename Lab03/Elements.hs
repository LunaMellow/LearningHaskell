-- Versions:
--   A: Pattern matching
--   B: Case expression
--   C: If expression
--   D: Maybe type
--   E: foldr
--   F: List comprehension

-- | Returns the first element of a list.
--
-- Examples:
--
-- >>> mheadA [1, 2, 3]
-- 1
--
-- >>> mheadA []
-- *** Exception: The list is empty
mheadA :: [a] -> a
mheadA [] = error "The list is empty"
mheadA (x:_) = x

-- | Returns the first element of a list using a case expression.
--
-- Examples:
--
-- >>> mheadB [1, 2, 3]
-- 1
--
-- >>> mheadB []
-- *** Exception: The list is empty
mheadB :: [a] -> a
mheadB xs = case xs of
    [] -> error "The list is empty"
    (x:_) -> x

-- | Returns the first element of a list using an if expression.
--
-- Examples:
--
-- >>> mheadC [1, 2, 3]
-- 1
--
-- >>> mheadC []
-- *** Exception: The list is empty
mheadC :: [a] -> a
mheadC xs = if null xs
    then error "The list is empty"
    else xs !! 0

-- | Returns the first element of a list wrapped in Maybe.
--
-- Examples:
--
-- >>> mheadD [1, 2, 3]
-- Just 1
--
-- >>> mheadD []
-- Nothing
mheadD :: [a] -> Maybe a
mheadD [] = Nothing
mheadD (x:_) = Just x

-- | Returns the first element of a list using foldr.
--
-- Examples:
--
-- >>> mheadE [1, 2, 3]
-- 1
--
-- >>> mheadE []
-- *** Exception: The list is empty
mheadE :: [a] -> a
mheadE xs = foldr (\x _ -> x) (error "The list is empty") xs

-- | Returns the first element of a list using list comprehension.
--
-- Examples:
--
-- >>> mheadF [1, 2, 3]
-- 1
--
-- >>> mheadF []
-- *** Exception: The list is empty
mheadF :: [a] -> a
mheadF xs = case [x | x <- xs] of
    [] -> error "The list is empty"
    (x:_) -> x

