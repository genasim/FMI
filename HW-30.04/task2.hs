import Data.List (sortOn)

main :: IO ()
main = do
    print $ (setupRobots [0, 1] "LR") 3
    print $ (setupRobots [-2, 0, 2] "RLL") 2
    print $ (setupRobots [-2, 0, 2] "RLL") 5
    print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 1
    print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 3
    print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 5

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
