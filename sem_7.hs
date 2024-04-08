import Data.Char (digitToInt, intToDigit)
import Data.List (sort, group, maximumBy)

main :: IO ()
main = do
--   print $ numDrink 761 3
--   print $ persistance 999
  print $ fibIter 100
  print $ splitEvery 7 "12586269025"
  print $ process "18540007707689471986333321219013"
  print $ aroundFib 100 25



numDrink :: Int -> Int -> Int
numDrink bottles exchange =
  if bottles < exchange
    then bottles
    else exchange + numDrink (bottles - exchange + 1) exchange





prodDigits :: Int -> Int
prodDigits = product . map digitToInt . show . abs

persistance :: Int -> (Int, [Int])
persistance n = helper n (0, [])
  where
    helper num (len, ys) = if prodDigits num < 10
        then (len+1, reverse (prodDigits num:ys))
        else helper (prodDigits num) (len + 1, prodDigits num:ys)




fibIter :: Integer -> Integer
fibIter n = helper n (0, 1)
  where
    helper 0 (a, _) = a
    helper n (a, b) = helper (n-1) (b, a+b)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery k xs = take k xs : splitEvery k (drop k xs)

process :: String -> (Char, Int)
process =
    (\ pair@(l,d) -> (intToDigit $ abs d, l))
    . maximum
    . map (\ xs@(x:_) -> (length xs, -(digitToInt x)))
    . group
    . sort


-- aroundFib :: Integer -> (Int -> [(Char, Int)])
-- aroundFib k = map process . splitEvery (fromIntegral k) . show . fibIter . fromIntegral
aroundFib :: Integer -> (Int -> [(Char, Int)])
aroundFib n k = (map process . splitEvery k . show . fibIter) n
