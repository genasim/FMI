main :: IO()
main = do
    print $ validBraces "()"
    print $ validBraces "[([)"
    print $ validBraces "())({}}{()][]["
    print $ validBraces "({})[({})]"
    print $ validBraces "([{)]}"
    print $ validBraces "([(){}])}"

validBraces :: String -> Bool
validBraces = go []
  where
    go [] [] = True
    go _ []  = False
    go stack (x:xs)
      | x `elem` "([{" = go (x:stack) xs
      | x == ')' && not (null stack) && head stack == '(' = go (tail stack) xs
      | x == ']' && not (null stack) && head stack == '[' = go (tail stack) xs
      | x == '}' && not (null stack) && head stack == '{' = go (tail stack) xs
      | otherwise = False
