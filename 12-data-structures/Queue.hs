module Queue (empty, isEmpty, enqueue, dequeue, front) where

data Queue a = Queue [a] [a]

empty :: Queue a
empty = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty (Queue {}) = False

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue f r) = Queue f $ x : r

front :: Queue a -> Maybe a
front (Queue [] []) = Nothing
front (Queue [] r) = front $ Queue (reverse r) []
front (Queue (x : _) []) = Just x

dequeue :: Queue a -> Queue a
dequeue q@(Queue [] []) = q
dequeue (Queue [] r) = dequeue $ Queue (reverse r) []
dequeue (Queue (x : xs) r) = Queue xs r
