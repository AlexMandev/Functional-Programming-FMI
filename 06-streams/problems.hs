-- 1. Да се дефинира функция, която приема списък и връща списък от всички подмножества на списъка

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x : xs) =
  map (x :) (subsets xs) ++ subsets xs

-- 2. Да се дефинира безкраен поток от всички наредени двойки естествени числа.

natPairs :: [(Int, Int)]
natPairs = [(x, y) | x <- [0 ..], y <- [0 ..]]

-- or better
natPairs' = [(x, x - y) | x <- [0 ..], y <- [0 .. x]]

-- 3. Да се дефинира безкраен поток от всички наредени двойки цели числа.
intPairs = [(x, y - x) | x <- [0 ..], y <- [0 .. x]]

-- 4. Да се дефинира безкраен списък от всички степени на двойката.
powersOf2 = [2 ^ n | n <- [0 ..]]

-- 5. Да се дефинира безкраен поток от числата на Фибоначи.
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

-- 6. Да се дефинира безкраен списък от факториелите на всички естествени числа.
factoriel 0 = 1
factoriel x = x * factoriel (x - 1)

factoriels = [factoriel x | x <- [0 ..]]

factoriels' = scanl (*) 1 [1 ..]

-- 7. Да се дефинира безкраен списък от всички триъгълни числа.
triangleNumbers = [sum [1 .. x] | x <- [1 ..]]

-- direct formula
triangleNumbers' = [n * (n + 1) `div` 2 | n <- [1 ..]]

-- or scanl works too

-- 8. Да се дефинира безкраен поток от всички Питагорови тройки.
pythagoreanTriples =
  [ (a, b, c) | c <- [1 ..], b <- [1 .. c], a <- [1 .. b], a ^ 2 + b ^ 2 == c ^ 2
  ]

-- 9. Да се дефинира безкраен поток от всички прости числа. За колко начина на имплементация се сещате?
isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = all (\x -> n `mod` x /= 0) [3, 5 .. n `quot` 2]

primes = [x | x <- [2 ..], isPrime x]

primes' = filter isPrime [2 ..]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x : xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)

primes'' :: [Int]
primes'' = sieve [2 ..]
