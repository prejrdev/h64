-- from learnyouhaskell: :: is read as "has type of"
min :: Integer -> Integer -> Integer
min a b = if a > b then b else a

-- class constraint, everything before => is the 
-- constraint. rust and haskell has 
-- a lot of similarities
bigger :: (Ord a) => a -> a -> Bool
bigger a b = a > b

month :: (Int) -> String
month 1 = "jan"
month 2 = "feb"
month 3 = "march"
month 4 = "april"
month 5 = "may"
month 6 = "june"
month 7 = "july"
month 8 = "august"
month 9 = "september"
month 10 = "october"
month 11 = "november"
month 12 = "december"
month n = month (n `mod` 12)
month _ = "unknown" --reaching for straws
-- odd: month 0 leaves a partial string in ghci
