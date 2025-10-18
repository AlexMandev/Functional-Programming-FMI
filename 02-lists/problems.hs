import Prelude

-- 1. Да се дефинира функция, която приема цяло число и проверява дали то е палиндром.
palindrome :: Int -> Bool
palindrome n = show n == reverse (show n)

-- 2. Да се дефинира функция, която приема две естествени числа x и n и връща по колко различни начина числото x може да се представи
-- като сума на различни числа, всяко повдигнато на степен n.

-- 3. Да се дефинира функция, която намира сумата на елементите в списък от цели числа.
sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs

-- 4. Да се дефинира функция, която приема два списъка като аргументи и залепя двата списъка един за друг. Да не се използва операторът ++
append :: [a] -> [a] -> [a]
append l [] = l
append [] l = l
append (x : xs) l2 = x : append xs l2

-- 5. Да се дефинират функциите take и drop от стандартната библиотека
take' :: Int -> [a] -> [a]
take' 0 l = []
take' n [] = []
take' n (x : xs)
  | n < 0 = []
  | otherwise = x : take' (n - 1) xs

drop' :: Int -> [a] -> [a]
drop' 0 l = l
drop' n [] = []
drop' n (x : xs)
  | n < 0 = x : xs
  | otherwise = drop' (n - 1) xs

-- 6. Да се дефинира функция, която приема елемент,
-- който ще наричаме разделител, от някакъв тип и списък с елементи от този тип и връща списък от списъци,
-- получен при "разделянето" на оригиналния списък спрямо разделителя.
split :: (Eq a) => a -> [a] -> [[a]]
split sep l = helper [] sep l
  where
    helper acc _ [] = [acc]
    helper acc sep (x : xs)
      | x == sep = if null acc then helper [] sep xs else acc : helper [] sep xs
      | otherwise = helper (acc ++ [x]) sep xs

-- 7. Да се дефинира функция, която приема списък и връща два списъка,
-- първият от които съдържа елементите на четните индекси, а вторият - елементите на нечетните индекси в оригиналния списък.
splitEvenOdd :: [a] -> ([a], [a])
splitEvenOdd l = helper ([], []) 0 l
  where
    helper acc pos [] = acc
    helper (f, s) pos (x : xs)
      | null l = (f, s)
      | even pos = helper (f ++ [x], s) (pos + 1) xs
      | otherwise = helper (f, s ++ [x]) (pos + 1) xs

-- 8. Да се дефинират следните две функции, които приемат списък като единствен аргумент:
-- - suffixes - връща списък от суфиксите на списъка;
-- - prefixes - връща списък от префиксите на списъка.
prefixes :: [a] -> [[a]]
prefixes l = reverse (helper [] [] l)
  where
    helper curr acc [] = curr : acc
    helper curr acc (x : xs) = helper (curr ++ [x]) (curr : acc) xs

suffixes :: [а] -> [[а]]
-- [1, 2, 3] -> [[1,2,3], [2,3], [3], []]
suffixes [] = [[]]
suffixes l = helper [] l
  where
    helper acc [] = acc ++ [[]]
    helper acc l = helper (acc ++ [l]) (tail l)

-- 9. Да се дефинира функция, която премахва всички последователни срещания на елементите в списък.
removeConsecutive :: (Eq a) => [a] -> [a]
removeConsecutive [] = []
removeConsecutive [x] = [x]
removeConsecutive (x : xs@(y : _)) =
  if x == y
    then removeConsecutive xs
    else x : removeConsecutive xs

-- 10. Да се напише функция, която приема списък от елементи и връща списък от списъци,
-- където всеки списък се състои от последователните срещания на един и същ елемент в списъка.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack l = helper [] l
  where
    helper acc [] = [acc]
    helper acc [x] = [acc ++ [x]]
    helper acc (x : xs@(y : _))
      | x == y = helper (acc ++ [x]) xs