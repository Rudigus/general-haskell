maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

-- Problem: it doesn't treat negative numbers for the first parameter
replicate' :: Int -> a -> [a]
replicate' 0 y = []
replicate' x y = [y] ++ replicate' (x - 1) y

-- Problem: It accepts floats for the first parameter, which, in my opinion, doesn't make much sense
replicate'' :: (Num i, Ord i) => i -> a -> [a]
replicate'' n x
    | n <= 0    = []
    | otherwise = x:replicate'' (n - 1) x

-- Could have omitted the first parameter in the first pattern match (empty list as second parameter)
take' :: Int -> [a] -> [a]
take' n [] = []
take' n (x:xs)
    | n <= 0    = []
    | otherwise = x:take' (n - 1) xs

take'' :: (Num i, Ord i) => i -> [a] -> [a]
take'' n _
    | n <= 0    = []
take'' _ []     = []
take'' n (x:xs) = x : take'' (n-1) xs

-- I made this and it's verbatim to the solution provided by the book. Bravo!
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Also verbatim
repeat' :: a -> [a]
repeat' x = x:repeat' x

-- Forgot again to omit the x and y parameters in the first two pattern matches (just change x and y to _)
zip' :: [a] -> [a] -> [(a, a)]
zip' x [] = []
zip' [] y = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

-- Pretty clever I'd say hehe
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = x == y || elem' x ys

-- More normal way, also almost the same as the book's solution
elem'' :: (Eq a) => a -> [a] -> Bool
elem'' _ [] = False
elem'' x (y:ys)
    | x == y    = True
    | otherwise = elem'' x ys

-- You can use :set +s command in ghci to see execution time and memory usage of anything you run, pretty useful
-- Just for fun, I tried testing elem, elem' and elem'' with the parameters: (10^x) [1..(10^x)]
-- where x is an arbitrary exponent I choose. Here's what I've found:
--
-- elem   - x = 10^9 - takes around 24 s and 72 GB!
-- elem'  - x = 10^8 - takes around 60 s and 25 GB
-- elem'' - x = 10^8 - takes around 40 s and 18 GB
--
-- Conclusions: default elem is vastly superior :')

-- Pretty elegant, but it isn't in-place and spends quite a bit of space, so bear that in mind.
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted