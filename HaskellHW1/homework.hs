isPrime :: Int -> Bool
isPrime x
    | x <= 1 = False
    | otherwise = helper 2 x
        where
            helper current n
                | current == n = True
                | mod n current == 0 = False
                | otherwise = helper (current + 1) n

truncatablePrime :: Int -> Bool
truncatablePrime x
    | (x>1) && (x<10) && (isPrime x) = True
    | isPrime x == False = False
    | otherwise = truncatablePrime (div x 10)

digitIn :: Int -> Int -> Bool
digitIn x y
    | x == 0 && y == 0 = True
    | otherwise = helper x y
        where
            helper x y
                | x == 0 = False
                | mod x 10  == y = True
                |otherwise = helper (div x 10) y

containsDigits :: Int -> Int -> Bool
containsDigits x y 
    | y == 0 = digitIn x y
    | otherwise = helper x y
        where
            helper x y
                | y == 0 = True
                | digitIn x (mod y 10) == False = False
                | otherwise = helper x (div y 10)

productOfDigits :: Int -> Int
productOfDigits x
    | x == 0 = 0
    |otherwise = helper x
        where
            helper x
                | x == 0 = 1
                | otherwise = (mod x 10) * helper (div x 10)

sumDel :: Int -> Int
sumDel x = helper x 1
    where
        helper x number
            | number == x = 0
            | mod x number == 0 = number + helper x (number + 1)
            | otherwise = helper x (number + 1)

interestingNumber :: Int -> Bool
interestingNumber x = x == sumDel (sumDel x)

quadrant :: Double -> Double -> Int
quadrant x y
    | x == 0 && y == 0 = 0
    | x <= 0 && y <= 0 = 3
    | x <= 0 && y >= 0 = 2
    | x >= 0 && y >= 0 = 1
    | x >= 0 && y <= 0 = 4
