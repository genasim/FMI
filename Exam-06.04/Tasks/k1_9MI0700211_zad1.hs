main :: IO()
main = do
    print $ sumOriginalNumbers 3 100
    print $ sumOriginalNumbers 25 250
    print $ sumOriginalNumbers 1000 2000
    print $ sumOriginalNumbers 15 2222

process :: Int -> Int
process n
  | even n    = n `div` 2
  | otherwise = 3 * n + 1

sequenceLength :: Int -> Int
sequenceLength 1 = 0
sequenceLength n = helper 0 n
    where
        helper res 1 = res
        helper res n = helper (res + 1) (process n)

isBeautiful :: Int -> Bool
isBeautiful n = odd $ sequenceLength n

isStrange :: Int -> Bool
isStrange n = even $ sequenceLength n

isOriginal :: Int -> Bool
isOriginal n
  | n <= 1 = False
  | otherwise  = isBeautiful n && isStrange (n-1) && isStrange (n+1)

sumOriginalNumbers :: Int -> Int -> Int
sumOriginalNumbers start finish
  | start < 1 || finish < 1 = error "Invalid input"
  | otherwise = sum [n | n <- [start..finish], isOriginal n]
  
  
  
