main :: IO()
main = do
    printList [3.1 .. 7.0]

printList :: [Int] -> IO()
printList list = 