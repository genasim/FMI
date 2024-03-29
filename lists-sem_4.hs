import Data.Char (digitToInt)

main :: IO ()
main = do
  print (divisors 36)
  print (isPrime 18)
  print (isPrime 19)
  print (generatePrimes 1 35)
  print (filterNotPrimes [1 .. 35])
  print (isAscending 2459)
  print (isImage [1,2,3,4,5] [3,4,5,6,7, 0])
  print (chunksOf 4 [1..22])

divisors :: Integer -> [Integer]
divisors 0 = []
divisors num = [d | d <- [1 .. num - 1], num `mod` d == 0]

isPrime :: Int -> Bool
isPrime num = [1, num] == [d | d <- [1 .. num], mod num d == 0]

generatePrimes :: Int -> Int -> [Int]
generatePrimes a b = [p | p <- [a .. b], isPrime p]

-- generatePrimes a b = filter isPrime [a .. b]

filterNotPrimes :: [Int] -> [Int]
filterNotPrimes xs = [p | p <- xs, isPrime p]
-- filterNotPrimes = filter isPrime

isAscending :: Int -> Bool
isAscending = helper . numToDigits
  where
    helper [_] = True
    helper (x1 : x2 : xs) = x1 < x2 && helper (x2 : xs)

    numToDigits :: Int -> [Int]
    numToDigits num = [digitToInt c | c <- show num]

isImage :: [Int] -> [Int] -> Bool
isImage [] [] = True
isImage [_] [_] = True
isImage (x1:x2:xs) (y1:y2:ys) = x1 - y1 == x2 - y2 && isImage (x2:xs) (y2:ys)
isImage _ _ = False

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)