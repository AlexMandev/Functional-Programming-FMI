import Data.Text (breakOn)

-- 28.1. Направете и тествайте собствена реализация на функциите map, filter
-- и fold(l,r,l1,r1).
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x : xs) = foldl' f (f acc x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc (x : xs) = f x (foldr' f acc xs)

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' _ [] = error "foldl1: empty list"
foldl1' f (x : xs) = foldl' f x xs

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' _ [] = error "foldr1: empty list"
foldr1' _ [x] = x
foldr1' f (x : xs) = f x (foldr1' f xs)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr' (\x acc -> f x : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr' (\x acc -> if p x then x : acc else acc) []

-- 28.2. Нека е даден списък l::[(Int,Int,Int)] с тройки (ai, bi, ci). С помощта
-- на map, fold и filter да се намерят:
-- a) Списъка от сумите на елементите на тройките [(ai + bi + ci)]
-- b) Тройка от сумите на отделните компоненти на елемнтите на l,
-- (Pai, Pbi, Pci)
-- c) Броя на тройките, за които ai + bi > ci
-- d) Дали има поне една тройка, за която ai = bi = ci (True или False)

-- a)
sumComponents :: [(Int, Int, Int)] -> [Int]
sumComponents = map (\(a, b, c) -> a + b + c)

-- b)
sumIntoPair = foldl' (\(a, b, c) (x, y, z) -> (a + x, b + y, c + z)) (0, 0, 0)

-- c)
validPairs :: [(Int, Int, Int)] -> Int
validPairs = foldl (\acc (a, b, c) -> if a + b > c then acc + 1 else acc) 0

-- d)
hasEqualTriple :: [(Int, Int, Int)] -> Bool
hasEqualTriple = foldl (\x (a, b, c) -> a == b && b == c || x) False

hasEqualTripleFilter lst = not (null (filter (\(a, b, c) -> a == b && b == c) lst))

-- 28.3 За списък от числа L да се намери списък с само с тези числа, които
-- съвпадат с поредния си номер в L. Например [1, 5, 3, 4, 2] → [1, 3, 4].
findMatchingWithIndex :: [Int] -> [Int]
findMatchingWithIndex l = foldr (\(a, b) acc -> if a == b then b : acc else acc) [] (zip l [1 ..])

-- 28.4 За списък от числа L да се намери списък със сумите на всички двойки
-- последователни елементи на L. Например [1, 5, 3, 4, 2] → [6, 8, 7, 6].

neighbourSum :: [Int] -> [Int]
neighbourSum [] = []
neighbourSum l = zipWith (+) l (tail l)

-- 28.5 С помощта на zipWith да се дефинира функция sums :: [Int] -> [Int],
-- която по списък от числа L = l1, l2, l3, ... намира списъка S = l1,(l1 +
-- l2)
-- ,(l1 + l2 + l3), ....
sums :: [Int] -> [Int]
sums [] = []
sums l@(x : xs) = x : zipWith (+) xs (sums l)

-- 28.6 Да се дефинира функция separate :: (a->Bool) -> [a] -> ([a],[a]),
-- която по предикат p и списък l връща двойката (pref,suf). pref е най-
-- дългият възможен префикс, такъв че всички негови елементи увовлет-
-- воряват p. suf е останалата част от списъка l. Например, separate even
-- [2,4,6,7,8,10] -> ([2,4,6],[7,8,10]). Забележка: вижте функция-
-- та break в Prelude.
separate :: (a -> Bool) -> [a] -> ([a], [a])
separate p lst = (pref, suf)
  where
    pref = takeWhile p lst
    suf = drop (length pref) lst

-- 28.7 Да се дефинира функция split :: (a->Bool) -> [a] -> [[a]], получ-
-- ваваща предикат p и списък l. Елементите на l, удовлетворяващи p, се
-- считат за “разделители” в l и списъкът се разделя на части, обособени
-- от тези разделители. Например:
-- split (==, ',') "part1 ,part2,part3" -> part1", "part2", "part3"]

split' :: (a -> Bool) -> [a] -> [[a]]
split' p lst = filter (not . null) (foldr step [[]] lst)
  where
    step x (y : ys)
      | p x = [] : y : ys
      | otherwise = (x : y) : ys

-- 28.9. (*) С помощта на foldr да се дефинира функция, която проверява да-
-- ли дали даден списък от числа l::[Int] е нареден във възходящ ред.
increasing :: [Int] -> Bool
increasing = snd . foldr check (Nothing, True)
  where
    check x (Nothing, _) = (Just x, True)
    check x (Just next, res) = (Just x, res && x <= next)
