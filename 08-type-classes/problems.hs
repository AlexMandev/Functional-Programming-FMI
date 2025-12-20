import Prelude hiding (Functor (..), Monoid (..), Semigroup, (<>))

-- 1 Да се дефинира типов клас Sizeable, който има единствен метод size :: a -> Int.
-- Да се създадат инстанции на класа Sizeable за типовете Int, Maybe a и [a].

class Sizeable a where
  size :: a -> Int
  size _ = 1

instance Sizeable Int where
  size _ = 4

instance (Sizeable a) => Sizeable (Maybe a) where
  size Nothing = 0
  size (Just a) = size a

instance (Sizeable a) => Sizeable [a] where
  size xs = sum $ map size xs

-- 2. Да се дефинира алгебричен тип данни Down, който съдържа елемент от някакъв тип, чиито елементи са сравними помежду си.
-- Да се създаде инстанция на класа Ord за типа Down, която "обръща" наредбата между елементите на типа, съдържащ се в Down.
newtype Down a = Down a
  deriving (Show, Eq)

instance (Ord a) => Ord (Down a) where
  Down x <= Down y = x >= y

-- 3. Да се създадат инстанции на следните типови класове за типа List:

-- Monoid - да се създаде overload на оператора <>, който "комбинира" две стойности от дадения тип;
-- Semigroup - да се създадат поотделно overload-и на функциите mempty (неутрален елемент), mappend (комбиниране на две стойности) и mconcat ("сплесква" списък от стойности до една стойност, като ги комбинира);
-- Show - форматира списъците по следния начин: (x1,x2,...,xN);
-- Eq - сравнява списъците поелементно;
-- Ord - сравнява списъците лексикографски;
-- Foldable - да се създаде overload на функцията foldr;
-- Functor - да се създаде overload на функцията fmap (подобно на map за вградените списъци).
-- Бонус: Напишете свои имплементации на горните типови класове.

data List a = Nil | Cons a (List a)

instance (Show a) => Show (List a) where
    show xs = "(" ++ show' xs ++ ")"
      where
        show' Nil = ""
        show' (Cons x Nil) = show x
        show' (Cons x xs) = show x ++ "," ++ show' xs

data NonEmpty a = a :| [a]

class Semigroup a where
  (<>) :: a -> a -> a  
  sconcat :: NonEmpty a -> a

instance Semigroup (List a) where
  (<>) :: List a -> List a -> List a
  Nil <> xs = xs
  xs <> Nil = xs
  (Cons x xs) <> ys = Cons x (xs <> ys)

  sconcat (x :| xs) = foldl (<>) x xs


class (Semigroup a) => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

  mconcat xs = foldr mappend mempty xs

class Eq' a where
  (===) :: a -> a -> Bool
  x === y = not $ x !== y 

  (!==) :: a -> a -> Bool
  x !== y = not $ x === y

instance (Eq a) => Eq (List a) where
  Nil == Nil = True
  Cons x xs == Cons y ys = x == y && xs == ys
  _ == _ = False

instance (Ord a) => Ord (List a) where
  Nil <= _ = True
  _ <= Nil = False
  Cons x xs <= Cons y ys = x <= y && xs <= ys

class Foldable' c where
  foldr' :: (a -> b -> b) -> b -> c a -> b

instance Foldable' List where
  foldr' :: (a -> b -> b) -> b -> List a -> b
  foldr' _ start Nil = start
  foldr' op acc (Cons x xs) = op x (foldr' op acc xs)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

-- 4. Да се дефинира типов клас Stream, който обобщава операциите, които можем да извършваме над даден "поточен тип",
-- независимо от елементите в него. Бихме искали да обобщим операциите за взимане на празен поток (empty),
-- добавянето на елемент в началото на поток (cons) и взимането на първия елемент в непразен поток (uncons).

class Stream s where
  empty :: s a
  cons :: a -> s a -> s a
  uncons :: s a -> Maybe (a, s a)

instance Stream [] where
  empty = []
  cons = (:)
  uncons lst =
    case lst of 
      [] -> Nothing
      (x : xs) -> Just (x, xs)

instance Stream List where
  empty = Nil
  cons = Cons
  uncons xs =
    case xs of
      Nil -> Nothing
      Cons h t -> Just (h, t)

-- 5. Нека е даден следния алгебричен тип за двоично дърво:

data BinTree a = Empty | Node a (BinTree a) (BinTree a)
  deriving (Show, Eq)

-- Да се създадат инстанции на типовите класове Functor и Foldable за типа BinTree

instance Functor BinTree where
  fmap :: (a -> b) -> BinTree a -> BinTree b
  fmap f Empty = Empty
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

instance Foldable' BinTree where
  foldr' :: (a -> b -> b) -> b -> BinTree a -> b
  foldr' _ acc Empty = acc
  foldr' op acc (Node x left right) = foldr' op (op x (foldr' op acc right)) left


--6. Нека е даден следния типов клас:

class Brzozowski r where
  nullable :: r a -> Bool
  derivative :: Eq a => a -> r a -> r a

-- и следните две дефиниции на АТД:

data DFA s a = DFA
  { start :: s
  , delta :: s -> a -> s
  , accept :: [s]
  }

data Regex a
  = REmpty
  | Epsilon
  | Atom a
  | KStar (Regex a)
  | Regex a :+ Regex a  -- обединение на регулярни изрази
  | Regex a :. Regex a  -- конкатенация на регулярни изрази

