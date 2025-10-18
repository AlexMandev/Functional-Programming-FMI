-- 23.1
length' :: [a] -> Int
length' [] = 0
length' (x : xs) = 1 + length' xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' x [] = False
elem' x (s : t) =
  if x == s
    then True
    else elem' x t

sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs

-- 23.2
count :: (Eq a) => a -> [a] -> Int
count x [] = 0
count x (s : t) =
  if x == s
    then 1 + count x t
    else count x t

-- 23.3
index :: (Eq a) => a -> [a] -> Int
index x [] = -1
index x (s : t) =
  if x == s
    then 0
    else 1 + index x t

-- 23.4
sublist :: (Eq a) => [a] -> [a] -> Bool
sublist [] _ = True
sublist (x : xs) l2 = elem' x l2 && sublist xs l2

-- 23.5
common :: (Eq a) => [a] -> [a] -> Int
common [] _ = 0
common _ [] = 0
common (x : xs) l2 =
  if elem' x l2
    then 1 + common xs l2
    else common xs l2

-- 23.6
duplicates :: (Eq a) => [a] -> Bool
duplicates [] = False
duplicates (x : xs) = elem' x xs || duplicates xs

-- 24.2
numberList :: Int -> [Int]
numberList x =
  if x < 10
    then [x]
    else numberList (x `quot` 10) ++ [x `mod` 10]

-- 25.1
say :: Int -> String
say 0 = "zero"
say 1 = "one"
say 2 = "two"
say 3 = "three"
say 4 = "four"
say 5 = "five"
say 6 = "six"
say 7 = "seven"
say 8 = "eight"
say 9 = "nine"
say _ = "not a single digit"

-- 25.2
longestCommonPrefix :: (Eq a) => [a] -> [a] -> Int
longestCommonPrefix [] _ = 0
longestCommonPrefix _ [] = 0
longestCommonPrefix (x : xs) (y : ys)
  | x == y = 1 + longestCommonPrefix xs ys
  | otherwise = 0

-- 25.3
countEvenOddl :: [Int] -> (Int, Int)
countEvenOddl [] = (0, 0)
countEvenOddl (x : xs)
  | even x = (1 + evens, odds)
  | otherwise = (evens, 1 + odds)
  where
    (evens, odds) = countEvenOddl xs

-- 25.4
pivot :: [Int] -> Int -> ([Int], [Int])
pivot [] _ = ([], [])
pivot (s : t) x
  | s < x = (s : lower, higher)
  | s > x = (lower, s : higher)
  | otherwise = (lower, higher)
  where
    (lower, higher) = pivot t x

-- 25.6
isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome s = head s == last s && isPalindrome (init (tail s))

-- 25.7
