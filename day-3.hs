is_bigger :: (Ord a) => a -> a -> Bool
is_bigger a b = a > b

is_smaller :: (Ord a) => a -> a -> Bool
is_smaller a b = a < b

--throwing error
errorful :: Bool -> [Char] -> Bool
errorful b msg
    | b = True
    | otherwise = error msg

--using guards
sum_int :: (Integral int) => int -> int
sum_int n 
    | n == 0 = 0
    | n > 0 = (sum_int n) +  n

m_gcd :: (Real a) => a -> a -> a
m_gcd a b 
    | a > b = m_gcd (a - b) b
    | a < b = m_gcd a (b - a)
    | a == b = a


bigger :: (Ord a) => a -> a -> a
bigger a b 
    | is_bigger a b = a
    | is_bigger b a = b

absolute :: (Integral a) => a -> a
absolute a
    | is_bigger a 0 = a
    | otherwise = a * (-1)


--alternative syntax to specify guards
smaller :: (Ord a) => a -> a -> a
a `smaller` b | is_smaller a b = a | otherwise = b

--where clause
distance (x1, y1) (x2, y2) = sqrt(dxx + dy2)
    where 
        dx = x2 - x1 
        dxx = dx * dx 
        dy = y2 - y1 
        dy2 = dy * dy


--let clause
area :: (Real a) => a -> a -> a
area a b = let area = a * b in area 


-- generalising induction
induction :: (Num t1, Ord t1) => t2 -> (t1 -> t2 -> t2) -> t1 -> t2
-- base has a type of t2. combine has a type of (t1 -> t2 -> t2), and n has a type of t1.
-- '(t1 -> t2 -> t2)' encompasses a function 
induction base combine n 
    | n == 0 = base
    | n > 0 = combine n (induction base combine (n-1)) --the guard here forces the constraint of Ord other `>` isnt available.

-- using anonymous functions in functions
sum_squares :: (Num a, Ord a) => a -> a --constrain a to types that belong to typeclasses of num and ord (order)
sum_squares n = induction 0 (\a b -> a*a + b) n

guardy :: a -> ( a -> Bool) -> (a -> a) -> a
guardy a b c
    | b (a) = a
    | not(b (a)) = c(a)
-- guardy 4 (\b -> b >= 5) (\b -> b * 2)
-- reflection: this isn't something that i can do in rust or javascript. i can see how i can compose
-- larger programs with literally just function application. observation: the structure in haskell is moved away from
-- the body of the function. also possibility of DSLs seem really interesting in haskell


--memoize
memo :: [Char] -> (() -> [Char])
memo msg = (\() -> msg)
-- usage:
-- print ((memo "yo")())


