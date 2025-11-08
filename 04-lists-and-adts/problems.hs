import Prelude

-- 1. Да се дефинира функцията zip, която приема два списъка и връща списък от наредени двойки от елементите им.
zip' :: [a] -> [b] -> [(a, b)]
zip' (x : xs) (y : ys) =
  if null xs || null ys
    then []
    else (x, y) : zip' xs ys

-- 2. Да се дефинира функция, която приема списък от списъци от произволен тип и връща най-дългия списък.
-- Ако има няколко списъка с еднаква дължина, да се върне първият такъв.
maxLengthList :: [[a]] -> [a]
maxLengthList [] = []
maxLengthList [l1] = l1
maxLengthList (x : y : xs) =
  if length x >= length y
    then maxLengthList (x : xs)
    else maxLengthList (y : xs)

-- 3. Да се дефинира функция, която имплементира алгоритъма за сортиране чрез пряка селекция.
remove :: (Eq a) => a -> [a] -> [a]
remove _ [] = []
remove x (y : ys) =
  if x == y
    then ys
    else remove x ys

-- 4. Да се дефинира функция, която приема два списъка с елементи от произволни типове и връща списък, представящ тяхното декартово произведение.
decart :: [a] -> [b] -> [(a, b)]
decart [] l2 = []
decart l1 [] = []
decart (x : xs) l2 = helper x l2 ++ decart xs l2
  where
    helper x [] = []
    helper x (y : ys) = (x, y) : helper x ys

-- 5. Да се дефинира функция, която приема списък с елементи от произволен тип и връща хистограма на списъка.
-- sХистограма ще наричаме списък от наредени двойки,
-- където първата компонента е колко пъти се среща в списъка даден елемент, а втората - кой е елементът.
addPair :: (Eq a) => a -> [(Int, a)] -> [(Int, a)]
addPair x [] = [(1, x)]
addPair x (p@(a, b) : ys) =
  if b == x
    then (a + 1, b) : ys
    else p : addPair x ys

histogram :: (Eq a) => [a] -> [(Int, a)]
histogram [] = []
histogram l = helper [] l
  where
    helper acc [] = acc
    helper acc (x : xs) = helper (addPair x acc) xs

-- 6. Да се дефинира функция, генерираща безкраен списък от двоичните записи на положителните естествени числа.
getBinaryString :: Int -> String
getBinaryString 0 = ""
getBinaryString a =
  if even a
    then getBinaryString (a `quot` 2) ++ "0"
    else getBinaryString (a `quot` 2) ++ "1"

buildBinary x = getBinaryString x : buildBinary (x + 1)

binary = buildBinary 1

-- 7. Да се дефинира алгебричен тип данни, представящ матрица, чиито елементи са числа, с произволни размерности. Да се дефинират следните функции:
-- isMatrix :: Matrix -> Bool, която проверява дали матрицата е валидна;
-- addMatrices :: Matrix -> Matrix -> Matrix;
-- multMatrices :: Matrix -> Matrix -> Matrix;
-- transpose :: Matrix -> Matrix.

newtype Matrix = Matrix [[Double]]
  deriving (Show, Eq)

-- Проверка дали матрицата е валидна
isMatrix :: Matrix -> Bool
isMatrix (Matrix []) = False
isMatrix (Matrix (r : rs))
  | null r = False
  | otherwise = checkRows (length r) rs
  where
    checkRows _ [] = True
    checkRows len (row : rows)
      | length row == len = checkRows len rows
      | otherwise = False
