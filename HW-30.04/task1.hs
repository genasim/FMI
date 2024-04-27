main :: IO ()
main = do
  print $ warmerAfter [20, 21, 20, 19, 18, 20, 25, 24, 23, 20, 26]
  print $ warmerAfter [0, 10, 20, 30]
  print $ warmerAfter [21, 22, 23]
  print $ warmerAfter [23, 24, 25, 21, 19, 23, 26, 23]

mapWithTail :: ([a] -> b) -> [a] -> [b]
mapWithTail func = map func . tails
  where
    tails = foldr (\x acc -> (x : head acc) : acc) [[]]

warmerAfter :: [Double] -> [Int]
warmerAfter temps@(t : ts) = mapWithTail largerThanNext temps
  where
    largerThanNext :: [Double] -> Int
    largerThanNext [] = 0
    largerThanNext [_] = 0
    largerThanNext (x : xs) =
      let greaterPart = dropWhile (<= x) xs
       in case greaterPart of
            [] -> 0
            (y : ys) -> length (y : takeWhile (<= x) xs)
