module LDE2 where

import Text.Read (readMaybe)

data LDE2 = LDE2 Int Int Int
  deriving (Show)

-- 1.1
extendedEuclid :: Int -> Int -> (Int, Int, Int)
extendedEuclid a 0 = (a, 1, 0)
extendedEuclid 0 a = (a, 0, 1)
extendedEuclid a b = (g, y, x - (a `div` b) * y)
  where
    (g, x, y) = extendedEuclid b (a `mod` b)

concreteSolution :: LDE2 -> Maybe (Int, Int)
concreteSolution (LDE2 a b c) =
  if c `mod` d == 0
    then Just (x * mult, y * mult)
    else Nothing
  where
    (d, x, y) = extendedEuclid a b
    mult = c `div` d

checkSolution :: (Int, Int) -> LDE2 -> Bool
checkSolution (x, y) (LDE2 a b c) = (a * x + b * y) == c

buildDiophantineSpace :: Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
buildDiophantineSpace x0 y0 u v k
  | k == 0 = (x0, y0) : buildDiophantineSpace x0 y0 u v 1
  | otherwise =
      (x0 + k * u, y0 - k * v)
        : (x0 - k * u, y0 + k * v)
        : buildDiophantineSpace x0 y0 u v (succ k)

-- 1.2
diophantine :: LDE2 -> [(Int, Int)]
diophantine (LDE2 a b c) =
  if c `mod` d == 0
    then buildDiophantineSpace x0 y0 u v 0
    else []
  where
    (d, x, y) = extendedEuclid a b
    u = b `div` d
    v = a `div` d
    mult = c `div` d
    x0 = x * mult
    y0 = y * mult

-- 1.3
prettyPrint :: LDE2 -> String
prettyPrint (LDE2 a b c) = show a ++ ".x " ++ sign ++ " " ++ show (abs b) ++ ".y = " ++ show c
  where
    sign = if b >= 0 then "+" else "-"

-- helpers
isWhiteSpace :: Char -> Bool
isWhiteSpace c = c == ' ' || c == '\n' || c == '\t'

removeSpaces :: String -> String
removeSpaces = filter (not . isWhiteSpace)

splitSides :: String -> Maybe (String, String)
splitSides str = case break (== '=') str of
  (left, '=' : right) -> Just (left, right)
  _ -> Nothing

parseRightSide :: String -> Maybe Int
parseRightSide = readMaybe

findOperator :: String -> Maybe (Char, Int)
findOperator [] = Nothing
findOperator (_ : str) = helper str 1
  where
    helper [] _ = Nothing
    helper (x : xs) i
      | x == '+' || x == '-' = Just (x, i)
      | otherwise = helper xs (i + 1)

parseCoefficient :: String -> Maybe Int
parseCoefficient str = case break (== '.') str of
  (num, '.' : _) -> readMaybe num
  _ -> Nothing

parseLeftSide :: String -> Maybe (Int, Int)
parseLeftSide str =
  case findOperator str of
    Nothing -> Nothing
    Just (op, idx) ->
      let (firstPart, secondPart) = splitAt idx str
          secondCoeff = drop 1 secondPart
       in case parseCoefficient firstPart of
            Nothing -> Nothing
            Just a -> case parseCoefficient secondCoeff of
              Nothing -> Nothing
              Just b -> Just (a, if op == '+' then b else -b)

-- 1.4
toLDE2 :: String -> Maybe LDE2
toLDE2 text =
  let cleaned = removeSpaces text
   in buildLDE2 (splitSides cleaned)
  where
    buildLDE2 Nothing = Nothing
    buildLDE2 (Just (left, right)) =
      buildWithC (parseRightSide right) left

    buildWithC Nothing _ = Nothing
    buildWithC (Just c) left =
      buildWithCoeffs (parseLeftSide left) c

    buildWithCoeffs Nothing _ = Nothing
    buildWithCoeffs (Just (a, b)) c = Just (LDE2 a b c)
