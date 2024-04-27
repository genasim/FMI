main :: IO()
main = do
    print $ frogJumps [1, 2, 1, 5]
    print $ frogJumps [1, 2, 2, -1]
    print $ frogJumps [1, -1]
    print $ frogJumps [2, -3, -1, 4, 5, 2, 3, 6, 3, 2, -2, 3, -1, -2, 8, 3, 4, 4, 3, 9, 3]
    print $ frogJumps [0, 9, 81, -82, 38, -50, -27, 29, -27, -88, -72, 54, -97]
    print $ frogJumps [30, 5, 17, 80, -4, 50, -15, 23, 84, -49, 3, 71, 97, -3, -24, 45, -38, -46, 19, 98, 65, 7, -31, -59, -51, -80, 42, -76, -90, -14, 0, 84, -27, -90, 36]
    print $ frogJumps [3, -69, 22, 32, -3, -50, 51, 75, -82, -67, -77, 10, 16, -72, -65, 2, -67, 2, -28, 37, 76, -72, -44, 51, -53, -8, -60, 74, -53, 95, 40, 97, 63, -56, 34, -32, 80, 46, -17, -95, 6]

frogJumps :: [Int] -> Int
frogJumps xs = helper xs [] 0
    where
        helper :: [Int] -> [Int] -> Int -> Int
        helper jumps visited idx
            | idx < 0 || idx >= length jumps = length visited
            | idx `elem` visited = -1
            | otherwise = let next = idx + (jumps !! idx) in
                helper jumps (idx : visited) next
