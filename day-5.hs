module Day5 where
    --constants
    greeting :: [Char]
    greeting = " world"

    greet :: [Char] -> [Char]
    greet m = m ++ greeting

    quotient_law :: () => Int -> Int -> Bool
    quotient_law a b = law == a where
        law = (quot a b) * b + (rem a b)

    remainder_law :: Int -> Int -> Bool
    remainder_law a b = law == a where
        divpart = div a b
        modpart = mod a b
        law = (divpart * a) + (modpart)

    --combining expressions with $, its an operator that
    --delays applying what is left from it until what is right
    -- of it is evaluated.
    c1 = (2 ^ ) $ (3 + 2)

    c2 = (2 ^) $ (+ 3) $ (2)

    maximum' :: [Int] -> Int
    maximum' [] = error "empty case"
    maximum' [a] = a 
    maximum' (a:b) =  --o(n-1)
        let rest = maximum' b in
            (if a > rest then a else rest)
    maximum' (a:b:c)
        | a > b = maximum' ([a] ++ c)
        | b > a = maximum' ([b] ++ c)

    fact n 
        | n == 0 = 1
        | n > 0 = n * fact (n - 1)

    induction base combo n
        | n == 0 = base
        | n > 0 = combo n (induction base combo (n-1))

    sum' n = induction 0 (+) n

    sqrt_sum n = induction 0 (\ a b -> (a * a) + b) n
