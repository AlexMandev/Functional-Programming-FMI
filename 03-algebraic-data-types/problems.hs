import Prelude hiding (repeat, Ordering)

-- 1. Да се дефинира функция, която приема елемент от произволен тип и връща безкраен списък, съдържащ този елемент:
repeat :: a -> [a]
repeat x = x : repeat x

-- 2. Да се дефинира функция, приемаща цяло число n
--  и генерира безкраен списък [n, n+1, ..]. За целта не използвайте генератор на списъци.
from :: Int -> [Int]
from n = n : from (n + 1)

-- 3. Да се дефинира безкраен списък от числата на Фибоначи.
fibs = fibsHelper 0 1
fibsHelper a b = a : (fibsHelper b (a + b)) 

-- 4. Да се дефинира алгебричен тип данни, представящ релации на наредба между елементи, като за два елемента x и y е вярно, че:

--     или x < y;
--     или x == y;
--     или x > y.

-- Да се дефинира функция, която сравнява два елемента от целочислен тип и връща стойност от горния АТД.
data Ordering = LessThan | Equal | GreaterThan
    deriving Show
cmpInt :: Int -> Int -> Ordering
cmpInt x y
  | x < y = LessThan
  | x == y = Equal
  | otherwise = GreaterThan

-- 5. Да се дефинира алгебричен тип данни, представящ фигура в равнината, 
-- която може да бъде триъгълник, квадрат или правилен многоъгълник, представена чрез броя страни,
-- които има, и дължините на тези страни. Да се дефинират следните функции:
    -- perimeter :: Shape -> Double;
    -- numberOfSides :: Shape -> Int;
    -- prettyPrint :: Shape -> String, където изходът трябва да е в следния формат:

data Shape = Square Double | Triangle Double Double Double | Polygon Int Double
    deriving (Show, Eq)

perimeter :: Shape -> Double
perimeter (Square a) = 4 * a
perimeter (Triangle a b c) = a + b + c
perimeter (Polygon n a) = fromIntegral n * a

numberOfSides :: Shape -> Int
numberOfSides (Square _) = 4
numberOfSides (Triangle _ _ _ ) = 3
numberOfSides (Polygon _ _) = 3

prettyPrint :: Shape -> String
prettyPrint (Square a) = "This figure is a square with sides of length " ++ show a
prettyPrint (Triangle a b c) = "This figure is a triangle with sides " ++ show a ++ ", " ++ show b ++ ", and " ++ show c
prettyPrint (Polygon n a) = "This figure is a regular polygon that has " ++ show n ++ " sides, each of length " ++ show a


-- 6. Да се дефинира алгебричен тип данни, представящ наредена двойка, 
---където двете компоненти могат да бъдат от произволен тип. Да се дефинират следните функции:
    -- myFst :: Pair a b -> a;
    -- mySnd :: Pair a b -> b;
    -- myRev :: Pair a b -> Pair b a;
    -- pairToTuple :: Pair a b -> (a,b);
    -- tupleToPair :: (a,b) -> Pair a b;
    -- cmpPair :: (Ord a, Ord b) => Pair a b -> Ordering, където Ordering е АТД от задача 04;
    -- pairsToList :: [Pair a b] -> Pair [a] [b].

data Pair a b = Pair a b
    deriving Show

myFst :: Pair a b -> a
myFst (Pair f _) = f

mySnd :: Pair a b -> b
mySnd (Pair _ s) = s

myRev :: Pair a b -> Pair b a
myRev (Pair a b) = Pair b a

pairToTuple :: Pair a b -> (a, b)
pairToTuple (Pair a b) = (a, b)

tupleToPair :: (a, b) -> Pair a b
tupleToPair (a, b) = Pair a b

cmpPair :: Pair Int Int -> Ordering
cmpPair (Pair x y) = cmpInt x y

pairsToList :: [Pair a b] -> Pair [a] [b]
pairsToList [] = Pair [] []
pairsToList ((Pair a b) : xs) =  Pair (a : as) (b : bs)
    where
        Pair as bs = pairsToList xs