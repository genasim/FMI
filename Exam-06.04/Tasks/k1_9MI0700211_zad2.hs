import Data.List (sort, group)
import Data.Char (digitToInt)

main :: IO()
main = do
    print $ getDistribution 123 (\ x -> x - 5)
    print $ getDistribution 232 (*3)
    print $ getDistribution 0 (2^)
    print $ getDistribution 881122 (\ x -> x - 10014)
    print $ getDistribution 881122 (10 `mod`)

getDistribution :: Int -> ((Int -> Int) -> [(Int,Int)])
getDistribution n = \func -> 
    (map (\x -> (digitToInt $ head x, length x))
    . group
    . sort 
    . show 
    . abs) $ func n