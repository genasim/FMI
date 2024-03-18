main :: IO()
main = do
    print (persistence 302)
    print (maxPersistenceMinSum 273 392)
    print (maxPersistenceMinSum 1000 2000)
    print (maxPersistenceMinSum 55 105)
    print (maxPersistenceMinSum 195 756)
    print (maxPersistenceMinSum 2 85)


persistence :: Int -> Int
persistence n = persistenceHelper n 0
  where
    persistenceHelper :: Int -> Int -> Int
    persistenceHelper n count
      | n < 10 = count
      | otherwise = persistenceHelper (productOfDigits n) (count + 1)

    productOfDigits :: Int -> Int
    productOfDigits 0 = 0
    productOfDigits n = helper n 1
        where
            helper 0 prod = prod
            helper n prod = if mod n 10 == 0
                then helper (div n 10) prod
                else helper (div n 10) (prod * mod n 10)



maxPersistenceMinSum :: Int -> Int -> Int
maxPersistenceMinSum start end = helper start start
    where
        helper :: Int -> Int -> Int
        helper curr result
            | curr > end = result
            | persistence curr > persistence result = helper (curr + 1) curr 
            | persistence curr == persistence result && sumDigits curr < sumDigits result = helper (curr + 1) curr
            | otherwise = helper (curr + 1) result

        sumDigits :: Int -> Int
        sumDigits n = helper n 0
            where
                helper :: Int -> Int -> Int
                helper 0 res = res
                helper num res = helper (num `div` 10) (res + (num `mod` 10))
