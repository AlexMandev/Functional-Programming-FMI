import Prelude hiding (Functor(..), Applicative(..), Monad(..))

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


-- 1. Създайте инстанции на горните типови класове за типа данни Maybe.
instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing = Nothing
    fmap f (Just x)= Just (f x)

instance Applicative Maybe where
    pure :: a -> Maybe a
    pure x = Just x

    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing <*> _ = Nothing
    _ <*> Nothing = Nothing
    Just f <*> Just x = Just (f x)

instance Monad Maybe where
    return = pure

    (>>=) :: (Maybe a) -> (a -> Maybe b) -> Maybe b
    Nothing >>= _ = Nothing
    Just x >>= f = f x

    (>>) :: (Maybe a) -> Maybe b -> Maybe b
    Nothing >> _ = Nothing
    _ >> rhs = rhs

-- 2. Създайте инстанции на горните типови класове за типа данни Either

instance Functor (Either c) where
    fmap :: (a -> b) -> Either c a -> Either c b
    fmap _ (Left x) = Left x
    fmap f (Right x) = Right (f x)

instance Applicative (Either c) where
    pure :: a -> Either c a
    pure x = Right x

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

