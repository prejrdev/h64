-- https://wiki.haskell.org/Keywords#.40
module Day6 where
    concat' [] = []
    concat' ([]:rest) = concat' rest --ignores empty element in the list
    concat' ((inner_head:inner_rest):list_rest) = inner_head:concat' (inner_rest:list_rest)

    zip' :: [a] -> [b] -> [(a, b)]
    zip' (a:as) (b:bs) = (a, b) : zip' as bs
    zip' _ _ = []

    --unzip' [] = ([], [])
    --unzip' (a:b:[]) = (a, b)
    --bug with typing
    unzip' [] = ([], [])
    unzip' ((a:b:_):rest) = (a:rest_of_a, b:rest_of_b) 
                        where (rest_of_a, rest_of_b) = unzip' rest --destructure the result

    --note: only works with lists, not tuples
    test_uz = [[1,2], [3,4], [5,6], [7,8]]
 
    dedupe :: (Eq a) => [a] -> [a]
    dedupe [] = []  
    dedupe [a] = [a]
    dedupe (a:b:rest)
        | a == b = dedupe (b:rest)
        | otherwise = a : dedupe(b:rest)
    
    main :: IO ()
    main = do
        putStrLn "hello world"
        putStrLn secondMessage
        where secondMessage = concat ["dog", "cat"]

    foldr' :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
    foldr' _fn base [] = base
    foldr' fn base (a:rest) = fn a (foldr' fn base rest) 

    --fold left
    foldl' _fn base [] = base
    foldl' fn base (a:rest) = foldl' fn (fn base a) rest

    sumd list = foldr' (+) 0 list

    prod list = foldr' (*) 1 list

    --take while provides flow control where it cease recursing after the prdicate
    --fails.  
    take_while _ [] = [] -- regardpess of the predicate, if the input is empty,  map to empty
    take_while p (a:rest)
        | p a = a : (take_while p rest)
        | otherwise = []

    permute [] = [[]]
    permute rest = [a:b | a <- rest, b <- permute (removeFirst a rest)]
        where removeFirst a [] = []
            -- if the  first and second matches, subsitute removeFirst
            -- with remaidner of list
              removeFirst a (b:bs) 
                | a == b = bs 
                | otherwise = b : removeFirst a bs