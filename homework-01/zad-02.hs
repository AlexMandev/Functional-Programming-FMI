-- зад. 2

data LDE2 = LDE2 Int Int Int
  deriving (Show)

data LDEN = LDEN [Int] Int
  deriving (Show, Eq)

-- всички двойки (i, j) | i < j
indexPairs :: Int -> [(Int, Int)]
indexPairs n = helper 0
  where
    helper i
      | i >= n = []
      | otherwise = makePairs i (i + 1) ++ helper (i + 1)

    makePairs i j
      | j >= n = []
      | otherwise = (i, j) : makePairs i (j + 1)

-- останалите замествания
allSubstitutions :: [Int] -> Int -> [[Int]]
allSubstitutions _ 0 = [[]]
allSubstitutions ys k = helper ys
  where
    helper [] = []
    helper (y : rest) = addToEach y (allSubstitutions ys (k - 1)) ++ helper rest

    addToEach x [] = []
    addToEach x (lst : lsts) = (x : lst) : addToEach x lsts

getAt :: [a] -> Int -> a
getAt (x : xs) 0 = x
getAt (x : xs) i = getAt xs (i - 1)

calculateNewConstant :: [Int] -> Int -> [Int] -> [Int] -> Int
calculateNewConstant coeffs c indices values =
  c - sumProducts coeffs indices values
  where
    sumProducts _ [] [] = 0
    sumProducts cs (i : is) (v : vs) = (getAt cs i) * v + sumProducts cs is vs

generateLDE2 :: [Int] -> LDEN -> [LDE2]
generateLDE2 ys (LDEN coeffs c) = concatMap processPair pairs
  where
    n = length coeffs
    pairs = indexPairs n

    processPair (i, j) =
      let otherIndices = filter (\x -> x /= i && x /= j) [0 .. n - 1]
          substitutions = allSubstitutions ys (length otherIndices)
       in map (makeLDE2 i j otherIndices) substitutions

    makeLDE2 i j otherIndices values =
      let newC = calculateNewConstant coeffs c otherIndices values
          a = getAt coeffs i
          b = getAt coeffs j
       in LDE2 a b newC
