listToNumber :: [Int] -> Int
listToNumber [] = 0
listToNumber (x:xs) = x * (10 ^ length xs) + listToNumber xs

suffix :: (Eq a) => [a] -> [a] -> Bool
suffix (x : xs) (y : ys)
    | length (x : xs) > length (y : ys) = False
    | (x : xs) == [] = [] == (y : ys)
    | length (x : xs) == length (y : ys ) = (x : xs) == (y : ys)
    | otherwise = suffix (x : xs) ys

isDigit x = (x >= 0 ) && (x <= 9)

timesInList :: Int -> [Int] -> Int
timesInList _ [] = 0
timesInList x (y : ys)
    | x == y = 1 + timesInList x ys
    |otherwise = timesInList x ys

occurrences :: [Int] -> [Int] -> [Int]
occurrences [] _ = []
occurrences _ [] = []
occurrences (x : xs) (y : ys) = timesInList x (y : ys) : occurrences xs (y : ys)

removeAt :: Int -> [a] -> [a]
removeAt _ [] = error "Empty"
removeAt x (y:ys)
    | x < 0 || x >= length (y : ys) = error "Indes Out of bounds"
    | x == 0 = ys
    | otherwise = y : removeAt (x - 1) ys
