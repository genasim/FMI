numStepCombinations :: Integer -> Integer
numStepCombinations n = memo !! fromIntegral n
  where
    memo = map numStep [0..]
    numStep 0 = 1
    numStep 1 = 1
    numStep n = memo !! (fromIntegral n - 1) + memo !! (fromIntegral n - 2)

main :: IO ()
main = do
  print (numStepCombinations 2)
  print (numStepCombinations 3)
  print (numStepCombinations 100)
