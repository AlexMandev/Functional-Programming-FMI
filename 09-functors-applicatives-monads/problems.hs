import Prelude hiding (Bifunctor(..), Functor(..), Applicative(..), Monad(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (find)

-- i'm writing the function types because i'm still learning, bear with it :)

-- 0. Дефинирайте свои варианти на типовите класове за функтор, апликативен функтор и монада.
--  Допълнително дефинирайте метода liftA2 :: (a -> b -> c) -> f a -> f b -> f c за класа Applicative,
--  използвайки другите му два метода.
--  За следващите задачи (без задача 09) използвайте именно тези дефиниции.

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 op lhs rhs =  (pure op <*> lhs) <*> rhs

class (Applicative m) => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    m >> n = m >>= \_ -> n

-- 1. Създайте инстанции на горните типови класове за типа данни Maybe.
instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing = Nothing
    fmap f (Just x)= Just (f x)

instance Applicative Maybe where
    pure :: a -> Maybe a
    pure = Just

    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing <*> _ = Nothing
    _ <*> Nothing = Nothing
    Just f <*> Just x = Just (f x)

instance Monad Maybe where
    return = pure

    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing >>= _ = Nothing
    Just x >>= f = f x

    (>>) :: Maybe a -> Maybe b -> Maybe b
    Nothing >> _ = Nothing
    _ >> rhs = rhs

-- 2. Създайте инстанции на горните типови класове за типа данни Either

instance Functor (Either c) where
    fmap :: (a -> b) -> Either c a -> Either c b
    fmap _ (Left x) = Left x
    fmap f (Right x) = Right (f x)

instance Applicative (Either c) where
    pure :: a -> Either c a
    pure = Right

    (<*>) :: Either c (a -> b) -> Either c a -> Either c b
    Right f <*> Right x = Right (f x)
    Left f <*> Right x = Left f
    Right f <*> Left x = Left x
    Left f <*> Left x = Left x

instance Monad (Either c) where
    return = pure

    (>>=) :: Either c a -> (a -> Either c b) -> Either c b
    (Left x) >>= f = Left x
    (Right x) >>= f = f x

    (>>) :: Either c a -> Either c b -> Either c b
    Left x >> _ = Left x
    _ >> rhs = rhs

-- 3. Да се дефинират алгебричните типове данни Sum и Product и да се създадат инстанции на класа Monoid.
-- Да се създадат инстанции на класовете Functor, Applicative, Monad за горните два АТД.

newtype Sum a = Sum a

newtype Product a = Product a

instance (Num a) => Semigroup (Sum a) where
    (<>) :: Sum a -> Sum a -> Sum a
    Sum x <> Sum y = Sum (x + y)

instance (Num a) => Monoid (Sum a) where
    mempty = Sum 0

instance (Num a) => Semigroup (Product a) where
    (<>) :: Product a -> Product a -> Product a
    Product x <> Product y = Product (x * y)

instance (Num a) => Monoid (Product a) where
    mempty = Product 1

instance Functor Sum where
    fmap :: (a -> b) -> Sum a -> Sum b
    fmap f (Sum x) = Sum (f x)

instance Functor Product where
    fmap :: (a -> b) -> Product a -> Product b
    fmap f (Product x) = Product (f x)

instance Applicative Sum where
    pure :: a -> Sum a
    pure = Sum

    (<*>) :: Sum (a -> b) -> Sum a -> Sum b
    Sum f <*> Sum x = Sum (f x)

    liftA2 :: (a -> b -> c) -> Sum a -> Sum b -> Sum c
    liftA2 f (Sum x) (Sum y) = Sum (f x y)

instance Applicative Product where
    pure :: a -> Product a
    pure = Product

    (<*>) :: Product (a -> b) -> Product a -> Product b
    Product f <*> Product x = Product (f x)

    liftA2 f (Product x) (Product y) = Product (f x y)

instance Monad Sum where
    return = pure

    (>>=) :: Sum a -> (a -> Sum b) -> Sum b
    Sum x >>= f = f x

instance Monad Product where
    return = pure

    Product x >>= f = f x


-- Създайте инстанции на горните типови класове за типа данни List a от миналия път.

data List a = Nil | Cons a (List a)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)


Cons x xs <++> Cons y ys = Cons x (xs <++> ys)

instance Applicative List where
    pure x = Cons x Nil

    (<*>) :: List (a -> b) -> List a -> List b
    Nil <*> _ = Nil
    Cons f fs <*> lst = fmap f lst <++> (fs <*> lst)

instance Monad List where
    return = pure
    
    Nil >>= _ = Nil
    Cons x xs >>= f = f x <++> (xs >>= f) 

-- 5. Дефинирайте типовия клас Bifunctor, подобен на Functor, който обаче разполага с две функции, които да прилага върху две различни стойности.
-- Създайте инстанции на типовете данни Either и (,) (наредена двойка) за типовия клас Bifunctor:
class Bifunctor bf where
    bimap :: (a -> c) -> (b -> d) -> bf a b -> bf c d 
    
    first :: (a -> c) -> bf a b -> bf c b
    first f = bimap f id

    second :: (b -> d) -> bf a b -> bf a d
    second = bimap id

instance Bifunctor Either where
    bimap f _ (Left x) = Left (f x) 
    bimap _ g (Right y) = Right (g y)

instance Bifunctor (,) where
    bimap f g (x, y) = (f x, g y)

-- 7. Създайте инстанции на типовите класове Functor, Applicative и Monad за типовия конструктор (->), където първият типов параметър е фиксиран.

instance Functor ((->) a) where
    fmap :: (b -> c) -> (a -> b) -> (a -> c)
    fmap f g = f . g

instance Applicative ((->) a) where
    pure :: b -> (a -> b)
    pure x = \_ -> x
    
    (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
    f <*> g = \x -> f x (g x)


instance Monad ((->) a) where
    return = pure

    (>>=) :: (a -> b) -> (b -> (a -> c)) -> (a -> c)
    f >>= g = \x -> g (f x) x

-- 8. Нека е дадена следната дефиниция на алгебричен тип данни:

newtype State s a = State {runState :: s -> (a, s)} 

-- който приема тип на състояние s и резултат от изчисление a. Да се създадат инстанции на типовите класове Functor, Applicative и Monad за State s, 
-- където типът на състоянието е фиксиран (защо?).

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State st) = State (\x -> let (y, s) = st x in (f y, s))

instance Applicative (State s) where
    pure :: a -> State s a
    pure x = State (\s -> (x, s))

    (<*>) :: State s (a -> b) -> State s a -> State s b
    State state1 <*> State state2 = State $ \s -> let (f, s') = state1 s
                                                      (x, s'') = state2 s'
                                                in (f x, s'')

    liftA2 :: (a -> b -> c) -> State s a -> State s b -> State s c
    liftA2 f (State state1) (State state2) = State $ \s -> let (x, s') = state1 s
                                                               (y, s'') = state2 s'
                                                            in (f x y, s'')

instance Monad (State s) where
    return = pure

    (>>=) :: State s a -> (a -> State s b) -> State s b 
    State state1 >>= f = State $ \s -> let (x, s') = state1 s 
                                           in runState (f x) s' 

-- 9.Използвайки State монадата от задача 08, напишете функция, реализираща алгоритъма за обхождане в дълбочина за граф.
-- Граф ще представяме чрез следния алгебричен тип данни:

data AdjacencyList a = AList
  { _value :: a,
    _adjacent :: [a]
  }
  deriving (Show, Eq, Ord)

newtype Graph a = Graph [AdjacencyList a]
    deriving (Show, Eq, Ord)   

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s () 
put newState = State $ \_ -> ((), newState)

dfs :: (Ord a) => Graph a -> a -> [a]
dfs graph startNode = fst (runState (dfsHelper graph startNode) Set.empty) 

dfsHelper :: (Ord a) => Graph a -> a -> State (Set a) [a]
dfsHelper graph currentNode = get >>= \visited -> 
    if currentNode `Set.member` visited
        then return []
        else put (Set.insert currentNode visited) >>= \_ ->
             let neighbors = getNeighbors graph currentNode
             in visitAll graph neighbors >>= \results ->
                return (currentNode : results)
  where
    visitAll :: Ord a => Graph a -> [a] -> State (Set a) [a]  
    visitAll _ [] = return []
    visitAll g (n:ns) = dfsHelper g n >>= \result1 ->
                        visitAll g ns >>= \result2 ->
                        return (result1 ++ result2)

getNeighbors :: Eq a => Graph a -> a -> [a]
getNeighbors (Graph adjLists) node = 
    case find (\(AList val _) -> val == node) adjLists of
        Just (AList _ neighbors) -> neighbors
        Nothing -> []

