import Data.List (sortOn)

------      TASK 1      ------

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




------      TASK 2      ------

type Robot = (Int, Char)

move :: Robot -> Robot
move (pos, 'R') = (pos + 1, 'R')
move (pos, 'L') = (pos - 1, 'L')

turn :: Robot -> Robot
turn (pos, 'R') = (pos, 'L')
turn (pos, 'L') = (pos, 'R')

handleCollision :: [Robot] -> [Robot]
handleCollision [] = []
handleCollision [r] = [r]
handleCollision (r1@(p1, _) : r2@(p2, _) : rs)
  | p1 == p2 = turn r1 : turn r2 : handleCollision rs
  | otherwise = r1 : handleCollision (r2 : rs)

processStep :: [Robot] -> [Robot]
processStep = handleCollision . sortOn fst . map move

setupRobots :: [Int] -> [Char] -> (Int -> [Int])
setupRobots ps ds = \t ->
    let robots = zip ps ds
        tick 0 rs = rs
        tick t rs = tick (t - 1) (processStep rs)
    in map fst $ tick t robots
