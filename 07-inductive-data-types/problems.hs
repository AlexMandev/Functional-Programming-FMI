-- 1. Да се дефинира алгебричен тип данни, представящ естествените числа в Пеановата аритметика.
-- Да се дефинират следните функции:
--     succ :: Nat -> Nat;
--     pred :: Nat -> Nat;
--     add :: Nat -> Nat -> Nat;
--     mult :: Nat -> Nat -> Nat.
--     fromInt :: Int -> Nat;
--     toInt :: Nat -> Int;
--     cmp :: Nat -> Nat -> Ordering.

data Nat = Zero | Succ Nat
  deriving (Show)

succ :: Nat -> Nat
succ nat = Succ nat

pred :: Nat -> Nat
pred Zero = error "No pred of zero"
pred (Succ nat) = nat

add :: Nat -> Nat -> Nat
add Zero (Succ num) = Succ num
add (Succ num) Zero = Succ num
add (Succ num1) (Succ num2) = add num1 (Succ (Succ num2))

mult :: Nat -> Nat -> Nat
mult Zero (Succ num) = Zero
mult (Succ num) Zero = Zero
mult (Succ num1) (Succ num2) = add (Succ num1) (mult (Succ num1) num2)

fromInt :: Int -> Nat
fromInt n
  | n < 0 = error "Nat defines only natural numbers"
  | n == 0 = Zero
  | otherwise = Succ (fromInt (n - 1))

toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ nat) = 1 + toInt nat

cmp :: Nat -> Nat -> Ordering
cmp Zero Zero = EQ
cmp Zero (Succ _) = LT
cmp (Succ _) Zero = GT
cmp (Succ nat1) (Succ nat2) = cmp nat1 nat2

-- 2. Да се дефинира алгебричен тип данни, представящ списък от елементи от произволен тип.
-- Да се дефинират следните функции:
-- isEmpty :: List a -> Bool;
-- headList :: List a -> Maybe a;
-- singleton :: a -> List a;
-- (+++) :: List a -> List a -> List a, който конкатенира два списъка;
-- reverseList :: List a -> List a;
-- fromList :: [a] -> List a;
-- toList :: List a -> [a];
-- mapList :: (a -> b) -> List a -> List b;
-- intersperse :: a -> List a -> List a.

data List a = Null | Cons a (List a)
  deriving (Show)

isEmpty :: List a -> Bool
isEmpty Null = True
isEmpty (Cons _ _) = False

headList :: List a -> Maybe a
headList Null = Nothing
headList (Cons a _) = Just a

singleton :: a -> List a
singleton a = Cons a Null

(+++) :: List a -> List a -> List a
Null +++ lst = lst
lst +++ Null = lst
(Cons a lst1) +++ lst2 = Cons a (lst1 +++ lst2)

reverseList :: List a -> List a
reverseList Null = Null
reverseList (Cons a lst) = reverseList lst +++ Cons a Null

fromList :: [a] -> List a
fromList = foldr Cons Null

toList :: List a -> [a]
toList Null = []
toList (Cons a lst) = a : toList lst

mapList :: (a -> b) -> List a -> List b
mapList _ Null = Null
mapList f (Cons a lst) = Cons (f a) (mapList f lst)

intersperse :: a -> List a -> List a
intersperse _ Null = Null
intersperse _ (Cons a Null) = Cons a Null
intersperse sep (Cons a lst) = Cons a (Cons sep (intersperse sep lst))

-- 3. Нека е дадена следният индуктивен алгебричен тип данни,
-- представящ израз, който се оценява до стойност от числен тип:

data Expr a
  = Constant a
  | Variable String
  | Expr a :+: Expr a
  | Expr a :*: Expr a
  deriving (Show, Eq, Ord)

-- Използвайки ваш АТД за речник, дефинирайте функция, която оценява такъв израз,
-- като оценката на променлива var се замества със стойността value на двойката ключ-стойност (var, value) в речника.
-- Ако такава няма, то няма как изразът да бъде оценен.
--     eval :: Num a => Dict String a -> Expr a -> Maybe a

newtype Dict k v = Dict [(k, v)]
  deriving (Show)

lookup' :: (Eq k) => k -> Dict k v -> Maybe v
lookup' _ (Dict []) = Nothing
lookup' key (Dict ((k, v) : kvps)) =
  if key == k then Just v else lookup' key (Dict kvps)

eval :: (Num a) => Dict String a -> Expr a -> Maybe a
eval _ (Constant x) = Just x
eval dict (Variable x) = lookup' x dict
eval dict (expr1 :+: expr2) = case (lhs, rhs) of
  (Just x, Just y) -> Just (x + y)
  _ -> Nothing
  where
    lhs = eval dict expr1
    rhs = eval dict expr2
eval dict (expr1 :*: expr2) = case (lhs, rhs) of
  (Just x, Just y) -> Just (x * y)
  _ -> Nothing
  where
    lhs = eval dict expr1
    rhs = eval dict expr2

-- Да се дефинира алгебричен тип данни, представящ дърво с произволен брой наследници.
-- Да се дефинират следните функции:
--  countNodes :: Tree a -> Int;
--  countLeaves :: Tree a -> Int;
--  contains :: Eq a => a -> Tree a -> Bool;
--  flatten :: Tree a -> [a].

-- data Tree a = Node a [Tree a]

-- commented so they dont clash (Node type constructor) with some of those below for the BST

-- countNodes :: Tree a -> Int
-- countNodes (Node _ xs) = 1 + sum (map countNodes xs)

-- countLeaves :: Tree a -> Int
-- countLeaves (Node _ []) = 1
-- countLeaves (Node _ xs) = sum (map countLeaves xs)

-- contains :: (Eq a) => a -> Tree a -> Bool
-- contains v (Node r xs) = r == v || any (contains v) xs

-- flatten :: Tree a -> [a]
-- flatten (Node a xs) = a : concatMap flatten xs

-- 5. Да се дефинира алгебричен тип данни, представящ двоично дърво с елементи от произволен тип. Да се дефинират следните функции:
-- countLeaves :: BinTree a -> Int;
-- height :: BinTree a -> Int;
-- mapBT :: (a -> b) -> BinTree a -> BinTree b;
-- inorder :: BinTree a -> [a];
-- preorder :: BinTree a -> [a];
-- toBST :: Ord a => [a] -> BinTree a;
-- isBalancedBST :: (Ord a, Bounded a) => BinTree a -> Bool.

data BinTree a = Empty | Node a (BinTree a) (BinTree a)
  deriving (Show)

tree :: BinTree Int
tree = Node 1 (Node 2 (Node 3 Empty Empty) Empty) (Node 4 (Node 6 Empty Empty) (Node 5 Empty Empty))

--     1
--   2     4
-- 3     6   5

countLeaves :: BinTree a -> Int
countLeaves Empty = 0
countLeaves (Node x left right) = 1 + countLeaves left + countLeaves right

height :: BinTree a -> Int
height Empty = 0
height (Node a left right) = 1 + max (height left) (height right)

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT _ Empty = Empty
mapBT f (Node a left right) = Node (f a) (mapBT f left) (mapBT f right)

inorder :: BinTree a -> [a]
inorder Empty = []
inorder (Node x left right) = inorder left ++ x : inorder right

preorder :: BinTree a -> [a]
preorder Empty = []
preorder (Node x left right) = x : preorder left ++ preorder right
