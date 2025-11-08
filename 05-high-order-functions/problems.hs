import Prelude hiding (all, any, concatMap, curry, filter, flip, foldl, foldr, iterate, map, succ, uncurry)

-- 0. Да се дефинира функция compose,
-- която приема две едноместни функции и връща нова функция, която е тяхната композиция.
-- Какъв трябва да е типът на compose?

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g x = f (g x)

-- 1. Да се дефинират следните функции от по-висок ред от стандартната библиотека (Prelude):
-- flip :: (a -> b -> c) -> b -> a -> c, която приема двуместна функция и "разменя" реда на аргументите ѝ;
-- map :: (a -> b) -> [a] -> b;
-- filter :: (a -> Bool) -> [a] -> [a];
-- foldr :: (a -> b -> b) -> b -> [a] -> b;
-- foldl :: (b -> a -> b) -> b -> [a] -> b;
-- all :: (a -> Bool) -> [a] -> Bool;
-- any :: (a -> Bool) -> [a] -> Bool;
-- concatMap :: (a -> [b]) -> [a] -> [b];
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c];
-- curry :: ((a,b) -> c) -> a -> b -> c;
-- uncurry :: (a -> b -> c) -> (a,b) -> c;
-- iterate :: (a -> a) -> a -> [a];
-- nub :: Eq a => [a] -> [a], която премахва всички повторения в списъка (от Data.List);
-- unfoldr :: (b -> Maybe (a,b)) -> b -> [a] (от Data.List).

flip :: (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x : xs) =
  if f x
    then x : filter f xs
    else filter f xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc [] = acc
foldr f acc (x : xs) = f x (foldr f acc xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl f acc (x : xs) = foldl f (f acc x) xs

all :: (a -> Bool) -> [a] -> Bool
all p = foldr step True
  where
    step x acc = p x && acc

any :: (a -> Bool) -> [a] -> Bool
any p = foldr step False
  where
    step x acc = p x || acc

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = foldr ((++) . f) []

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f l1 l2 = foldr (\(a, b) acc -> f a b : acc) [] (zip l1 l2)

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

nub :: (Eq a) => [a] -> [a]
nub = foldl step []
  where
    step acc x
      | x `elem` acc = acc
      | otherwise = acc ++ [x]

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f x = case f x of
  Nothing -> []
  Just (a, b) -> a : unfoldr f b

-- 2. Да се дефинира оператор (&&&),
-- който приема две едноместни функции и някакъв аргумент x и връща наредена двойка от резултатите от прилагането на функциите върху x.

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(f &&& g) x = (f x, g x)

-- 3. Да се напише функция,
-- която приема едноместна функция и списък от някакви елементи и връща списък от всички неподвижни точки на функцията в подадения интервал.

fixedPoints :: (Eq a) => (a -> a) -> [a] -> [a]
fixedPoints f = filter (\x -> f x == x)

-- 4. Да се дефинира функция, която приема списък от функции и връща тяхната композиция.
-- Напишете два варианта на функцията - използвайки рекурсия и използвайки функции от по-висок ред.
composeFunctionsRec :: [a -> a] -> (a -> a)
composeFunctionsRec [] = id
composeFunctionsRec (x : xs) = x . (composeFunctionsRec xs)

composeFunctions :: [a -> a] -> (a -> a)
composeFunctions = foldr compose id

-- 5. Да се дефинира функция, която имплементира алгоритъма за сортиране чрез пряка селекция, използвайки само функции от по-висок ред.
minimum' :: (Ord a) => [a] -> a
minimum' = foldl1 min

removeFirst :: (Eq a) => a -> [a] -> [a]
removeFirst x (y : ys) =
  if x == y
    then ys
    else x : removeFirst x ys

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort lst = x : selectionSort (removeFirst x lst)
  where
    x = minimum' lst

-- 6. Да се дефинира функция, която имплементира алгоритъма за бързо сортиране
-- Бонус: Функцията да приема двуместен предикат, който да дефинира наредбата между елементите в списъка
quickSort :: (a -> a -> Bool) -> [a] -> [a]
quickSort p [] = []
quickSort p (x : xs) = quickSort p smaller ++ [x] ++ quickSort p larger
  where
    smaller = filter (`p` x) xs
    larger = filter (not . (`p` x)) xs