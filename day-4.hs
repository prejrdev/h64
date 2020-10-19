mults_of_3_5 :: (Integral a) => () -> [a]
mults_of_3_5 () = [c | c <- [1..999], c `mod` 5 == 0 || c `mod` 3 == 0]


take_3 :: [a] -> [a]
take_3 [] = []
take_3 (a:b:c:_res) = [b,c,a] ++ _res
--take_3 [a:b:c] = [c, b, a]

head' :: [a] -> a
head' [] = error "cant be empty"
head' (a:[]) = a
head' (a:_b) = a

tail' :: [a] -> a
tail' [] = error "cant be empty"
tail' (a: []) = a
tail' (_a:b) = tail' b

unhead' :: a -> [a]
unhead' a = [a]

--idomatic way in haskell, slow
fib' :: (Num a) => [a]
fib' = 0 : 1 : zipWith (+) fib' (tail fib')
-- invoke with 'fib' !! amount' its really slow

-- from stackexchange
fibo :: Integral x => x -> [x]
fibo n = fiboHelper [0,1] 0 1 n where
    fiboHelper l x y 0 = l
    fiboHelper l x y n = fiboHelper (l ++ [y+x] ++ [y+x+y]) (x+y) (y+x+y) (n-1)

even_fib n = [a | a <- fibo n, a `mod` 2 == 0] 
even_sum n = sum (even_fib n)

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)

--need a constraint because int could be < 0
fib_take :: Int -> [Integer] 
fib_take n = [memoized_fib(a) | a <- [1..n]]

-- project euler question
prob_2 = sum (filter (\n -> n < 4000000 && n `mod` 2 == 0) (fib_take 300))

-- quick sort
quick_sort :: [a1] -> [a2]
quick_sort [] = [] --empty list case
quick_sort (first:rest) = []

-- show is a function that maps a to string.
inspect_list :: [a] -> String  
inspect_list xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [_x] = "a singleton list."  
          what xs = "a longer list. length is " ++ show (len xs)
          len [] = 0
          len [_a] = 1
          len (_a:b)  = 1  + (len b)


even_or_odd n = "the number is " ++ case n `mod` 2 of
     0 -> "even"
     1 -> "not even"



