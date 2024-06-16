main :: IO ()
main = do
  print $ minAmountsForGame (Game 1 [[Blue 3, Red 4], [Red 1, Green 2, Blue 6], [Green 2]])
  let games1 = [ Game 1 [[Blue 3, Red 4], [Red 1, Green 2, Blue 6], [Green 2]],
                Game 2 [[Blue 1, Green 2], [Green 3, Blue 4, Red 1], [Green 1, Blue 1]],
                Game 3 [[Green 8, Blue 6, Red 20], [Blue 5, Red 4, Green 13], [Green 5, Red 1]],
                Game 4 [[Green 1, Red 3, Blue 6], [Green 3, Red 6], [Green 3, Blue 15, Red 14]],
                Game 5 [[Red 6, Blue 1, Green 3], [Blue 2, Red 1, Green 2]]
              ]
  let games2 = [
        Game 1 [[Red 18,Green 8,Blue 7],[Red 15,Blue 4,Green 1],[Green 2,Red 17,Blue 6],[Green 5,Blue 1,Red 11],[Red 18,Green 1,Blue 14],[Blue 8]],
        Game 2 [[Blue 16,Green 12,Red 3],[Blue 13,Red 2,Green 8],[Green 15,Red 3,Blue 16]],
        Game 3 [[Green 6,Red 15],[Green 1,Red 4,Blue 7],[Blue 9,Red 7,Green 8]],
        Game 4 [[Red 8,Blue 2],[Red 11,Blue 5,Green 1],[Red 12,Green 1,Blue 5],[Blue 1],[Blue 2,Red 9]],
        Game 5 [[Blue 9,Red 3,Green 12],[Green 3,Red 4,Blue 17],[Blue 15,Green 2,Red 5],[Blue 3,Green 5,Red 6],[Red 6,Blue 4,Green 7],[Green 3,Blue 10]],
        Game 6 [[Red 11,Blue 2,Green 6],[Blue 2,Red 9,Green 4],[Blue 3,Red 12,Green 8],[Red 5,Green 11,Blue 4],[Blue 2,Red 9,Green 13],[Red 15,Blue 3,Green 7]],
        Game 7 [[Red 2,Green 9,Blue 12],[Blue 14,Green 1,Red 6],[Blue 7,Green 9],[Green 9,Red 8,Blue 4],[Red 5,Green 3,Blue 16],[Red 4,Green 8]],
        Game 8 [[Red 11,Green 12,Blue 1],[Red 4,Green 7],[Red 11,Green 6],[Green 17],[Green 15,Red 1]],
        Game 9 [[Red 1,Green 1,Blue 12],[Green 3,Red 12,Blue 6],[Red 14,Blue 1],[Blue 9,Red 1,Green 3]],
        Game 10 [[Red 1,Blue 4],[Blue 3,Green 4],[Green 3,Red 3,Blue 8],[Blue 2,Red 3],[Green 3,Red 4,Blue 3]],
        Game 11 [[Blue 8,Red 1],[Green 8,Red 1,Blue 1],[Green 13,Red 9,Blue 6]],
        Game 12 [[Red 2,Blue 2,Green 1],[Red 3,Green 1],[Blue 1,Green 3]],
        Game 13 [[Green 12,Blue 4],[Red 2,Blue 2,Green 8],[Green 6,Red 3],[Red 3,Green 5],[Green 9,Blue 7,Red 1]],
        Game 14 [[Red 1,Green 7],[Green 5,Red 12,Blue 10],[Red 9,Blue 11,Green 7],[Blue 7,Red 3,Green 9]],
        Game 15 [[Green 7,Blue 1],[Red 1,Green 2,Blue 1],[Green 7]],
        Game 16 [[Green 1,Blue 1],[Blue 2,Green 4,Red 2],[Green 2,Blue 2]],
        Game 17 [[Red 6,Green 11,Blue 7],[Blue 1,Green 13,Red 4],[Green 4,Blue 6,Red 7]],
        Game 18 [[Red 2,Blue 8],[Red 7,Blue 11],[Green 1,Blue 16,Red 7],[Blue 18,Green 1,Red 14]],
        Game 19 [[Red 2,Blue 2],[Green 1,Red 6],[Green 1,Red 3,Blue 2]],
        Game 20 [[Red 6,Blue 2,Green 5],[Red 4,Blue 1,Green 9],[Blue 3,Red 2,Green 9],[Red 8,Green 12,Blue 5]],
        Game 21 [[Red 6,Blue 7],[Blue 3,Red 16,Green 2],[Blue 2,Red 13],[Blue 3,Red 11,Green 3],[Green 1,Red 18,Blue 6],[Red 12,Blue 5,Green 2]],
        Game 22 [[Red 9,Blue 6,Green 14],[Blue 1,Green 5,Red 13],[Red 6],[Red 18,Green 4],[Blue 2,Green 10,Red 16],[Red 1,Green 18,Blue 1]]
        ]
  print $ minRequiredSum games1
  print $ minRequiredSum games2



type Number = Int
type Amount = Integer

data Sample = Blue Amount | Red Amount | Green Amount
  deriving (Show, Eq)

data Game = Game Number [[Sample]]
  deriving (Show)

computeDegree :: Amount -> Amount -> Amount -> Amount
computeDegree red green blue = red * green * blue

extractAmounts :: [Sample] -> (Amount, Amount, Amount)
extractAmounts samples = (reds, greens, blues)
  where
    reds = sum [amount | Red amount <- samples]
    greens = sum [amount | Green amount <- samples]
    blues = sum [amount | Blue amount <- samples]

minAmountsForGame :: Game -> (Amount, Amount, Amount)
minAmountsForGame (Game _ rounds) = (maximum reds, maximum greens, maximum blues)
  where
    amounts = map extractAmounts rounds
    reds = [r | (r, _, _) <- amounts, r > 0]
    greens = [g | (_, g, _) <- amounts, g > 0]
    blues = [b | (_, _, b) <- amounts, b > 0]

minRequiredSum :: [Game] -> Amount
minRequiredSum games = sum (map (computeDegreeForGame . minAmountsForGame) games)
  where
    computeDegreeForGame (red, green, blue) = computeDegree red green blue

