main :: IO()
main = do
    print $ longestList [[1,2], [3,4,5], [1], [100,200,300,900]]
    print $ longestList [[5], [12,128,11,441,21,8], [], [12,64,261,7]]
    print $ longestList [[1,2], [3], [4,5,6], [7,8,9]]

longestList :: [[a]] -> [a]
longestList = foldr longer []

longer :: [a] -> [a] -> [a]
longer xs ys = if length xs <= length ys then ys else xs