-- day one
-- learning about:
-- how to comment in haskell. installing it
-- chapter one of learn you haskell
-- and chapter 2

double x = x + x

to_even n = (if (mod n 2) == 0 then n else n + 1) 

-- in haskell everything is an expression. 
-- there are no statements?!
-- so to make use of this, it changes the style
-- of constructing the if-else expression:l 

to_odd_one n = (if (n `mod` 2) == 0 then n else n + 1) + 1

-- sentence constructor using lists

first_elm list = list !! 1

consume list = drop 1 list

mappy list = to_odd_one (first_elm (consume list))

mult64 :: (Num a, Enum a) => a -> [a]
mult64 n = [64*x | x <- [1..n]]

filter512 list = [x | x <- list, x `mod` 512 == 0]

filter'512 list = [x | x <- list, x `mod` 512 /= 0]

--a more complete example of list comprehesion. 
right_tr_perimeter_solver :: (Num c, Eq c, Enum c) => c -> [(c, c, c)]
right_tr_perimeter_solver p  = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == p]
