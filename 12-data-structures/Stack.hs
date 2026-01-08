module Stack (Stack, empty, isEmpty, push, pop, peek) where

newtype Stack a = Stack [a]

empty :: Stack a
empty = Stack []

isEmpty :: Stack a -> Bool
isEmpty (Stack xs) = null xs

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

pop :: Stack a -> Stack a
pop (Stack []) = Stack []
pop (Stack (x : xs)) = Stack xs

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x : _)) = Just x

toList :: Stack a -> [a]
toList (Stack xs) = xs

fromList :: [a] -> Stack a
fromList = Stack

instance Functor Stack where
  fmap :: (a -> b) -> Stack a -> Stack b
  fmap f (Stack xs) = Stack $ map f xs

instance Foldable Stack where
  foldr :: (a -> b -> b) -> b -> Stack a -> b
  foldr op nv (Stack xs) = foldr op nv xs
