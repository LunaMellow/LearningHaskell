
-- Run:     stack runhaskell OldestStudents.hs < OldestStudents.txt
-- Test:    doctest OldestStudents.hs

-- | Complexity:
--
-- parseLine runs O(1) splitting a string into three parts)
-- map paseLine (lines input) runs in O(N) since lines and map both traverse the input once
-- oldest runs in O(N) using a single pass foldl
-- Overall: O(N) + O(N) = O(N)

import System.IO (getContents)

-- | Parses a line containing a student's name, surname, and age.
--
-- >>> parseLine "Alice Cooper 25"
-- ("Alice","Cooper",25)
--
-- >>> parseLine "Bob Marley 30"
-- ("Bob","Marley",30)
parseLine :: String -> (String, String, Int)
parseLine line = case words line of
    [name, lastname, age] -> (name, lastname, read age)

-- | Finds the oldest students and counts them.
--
-- >>> oldest [("Alice","Cooper",25),("Bob","Marley",23),("Charlie","Chaplin",25)]
-- (25,2)
--
-- >>> oldest []
-- (0,0)
oldest :: [(String, String, Int)] -> (Int, Int)
oldest = foldl update (0, 0) 
    where
    update (maxAge, count) (_, _, age)
        | age > maxAge = (age, 1)
        | age == maxAge = (maxAge, count + 1)
        | otherwise = (maxAge, count)

-- Main
main :: IO ()
main = do
    input <- getContents
    let students = map parseLine (lines input)
        (maxAge, count) =  oldest students
    print count

