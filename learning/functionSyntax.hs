-- Pattern Matching

gamblingMachine :: Int -> String
gamblingMachine 7 = "You won the jackpot!"
gamblingMachine 13 = "You won a big prize!"
gamblingMachine 42 = "We don't have anything to give you which you don't already have"
gamblingMachine 1337 = "LEET RULEZ"
gamblingMachine x = "Better luck next time :)"

tail' :: [a] -> [a]
tail' [] = error "empty list"
tail' (_:xs) = xs

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Guards

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

-- Where

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

initials' :: String -> String -> String
initials' firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [w / h ^ 2 | (w, h) <- xs]

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- Let

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

calcBmis'' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis'' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcBmis''' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis''' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- Case Expressions

tail'' :: [a] -> [a]
tail'' xxs = case xxs of [] -> error "empty list"
                         (_:xs) -> xs

tail''' :: [a] -> [a]
tail''' xxs = localTail xxs
    where localTail [] = error "empty list"
          localTail (_:xs) = xs

tail'''' :: [a] -> [a]
tail'''' xxs = let localTail [] = error "empty list"
                   localTail (_:xs) = xs
               in localTail xxs

-- You can omit the last semicolon (;) in the let binding / expression
tail''''' :: [a] -> [a]
tail''''' xxs = let { localTail [] = error "empty list"; localTail (_:xs) = xs; } in localTail xxs