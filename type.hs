removeSpaces :: String -> String
removeSpaces st = [ c | c <- st, c /= ' ']

crazyNumberMachine :: Num a => a -> a -> a -> a
crazyNumberMachine a b c = a + 2 * b + 3 * c + a * b * c

factorial :: Integer -> Integer
factorial n = product [1..n]

factorial' :: Int -> Integer
factorial' n = product (take n [1..])