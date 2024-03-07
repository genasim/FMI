main :: IO()
main = do
    print (gcdIf 104 18)
    print (gcdDec 104 18)
    print (sumDigitsRec 12345)
    print (sumDigitsIter 12345)
    print (reversed 12345)
    print (2 `power` 5)
    print (isAscending 13589)
    print (isAscending 12634)
    print (countOccurences 2 13452480042248)
    print (isPrime 53)
    print (isPrime 51)

-- Recursive with ifs
gcdIf :: Int -> Int -> Int
gcdIf a b =
    if b == 0 then a
    else gcdIf b (a `mod` b)

-- Recursive with declarations
gcdDec :: Int -> Int -> Int
gcdDec a 0 = a
gcdDec a b = gcdDec b (a `mod` b)



-- sumDigitsRec :: Int -> Int
-- sumDigitsRec num = 
--     if num < 10 then 1
--     else num `mod` 10 + sumDigitsRec (num `div` 10)

sumDigitsRec :: Int -> Int
sumDigitsRec num
    | num < 10 = 1
    | otherwise = num `mod` 10 + sumDigitsRec (num `div` 10)

sumDigitsIter :: Int -> Int
sumDigitsIter num = accumulate num 0
    where accumulate num sum
            | num < 10 = sum + num
            | otherwise = accumulate (num `div` 10) (sum + (num `mod` 10))

reversed :: Int -> Int
reversed num = accumulator num 0
  where
    accumulator :: Int -> Int -> Int
    accumulator 0 res = res
    accumulator n res = accumulator (n `div` 10) (concatAtBack res (n `mod` 10))

    concatAtBack :: Int -> Int -> Int
    concatAtBack num digit = num * 10 + digit


power :: Int -> Int -> Int
power base 0 = 1
power base k
    | even k = power (base * base) (k `div` 2)
    | otherwise = base * power base (k - 1)


isAscending :: Int -> Bool
isAscending 0 = True
isAscending num = (num `mod` 10) > ((num `div` 10) `mod` 10) && isAscending (num `div` 10)


countOccurences :: Int -> Int -> Int
countOccurences d num = helper d num 0
    where
        helper d num count
            | num == 0 = count
            | (num `mod` 10) == d = helper d (div num 10) count + 1
            | otherwise = helper d (div num 10) count


isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = helper 2
        where helper d
                | n == d         = True
                | n `mod` d == 0 = False
                | otherwise      = helper (d + 1)