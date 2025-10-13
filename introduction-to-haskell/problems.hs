import Prelude hiding (exp, gcd, lcm, pred, succ)

-- 1. Да се дефинират функции,
-- които пресмятат периметъра и лицето на триъгълник в равнината по подадени негови дължини на три страни.
trianglePerimeter :: Double -> Double -> Double -> Double
trianglePerimeter a b c = a + b + c

triangleArea :: Double -> Double -> Double -> Double
triangleArea a b c = sqrt (semiperimeter * d1 * d2 * d3)
  where
    semiperimeter = trianglePerimeter a b c
    d1 = semiperimeter - a
    d2 = semiperimeter - b
    d3 = semiperimeter - c

-- 2. Да се дефинират същите функции от Задача 01, които работят върху три точки в равнината вместо дължини на страни.

-- TO DO

-- 3. Да се дефинира функция, която приема като единствен параметър данните на студент от ФМИ
-- - име, факултетен номер, специалност и курс, и връща форматиран низ с информацията на студента.
printStudent :: (String, String, String, Int) -> String
printStudent (name, fn, spec, year) =
  "This is "
    ++ name
    ++ " with a faculty number of "
    ++ fn
    ++ " who is in year "
    ++ show year
    ++ " of "
    ++ spec

-- 4. Да се дефинира функцията succ, която приема естествено число и връща естественото число, получено чрез прибавяне на единица.
-- Без да използвате други функции освен указаната, дефинирайте следните функции:
--     pred - за естествено число n връща n-1. Ако n==0, връща 0;
--     add - прибавя две естествени числа;
--     mult - умножава две естествени числа;
--     exp - повдига първия си аргумент на степен втория си аргумент.

succ :: Int -> Int
succ x = x + 1

pred :: Int -> Int
pred 0 = 0
pred x = helper 0 x
  where
    helper :: Int -> Int -> Int
    helper acc n
      | n <= 0 = 0
      | succ acc == n = acc
      | otherwise = helper (succ acc) n

add :: Int -> Int -> Int
add 0 m = m
add n m = succ (add (pred n) m)

mult :: Int -> Int -> Int
mult 0 m = 0
mult n m = add m (mult (pred n) m)

exp :: Int -> Int -> Int
exp n 0 = 1
exp n m = mult n (exp n (pred m))

-- 5. Да се дефинира функция, която намира сбора от цифрите на произволно цяло число.
digitSum :: Int -> Int
digitSum 0 = 0
digitSum x = abs x `mod` 10 + digitSum (x `quot` 10)

-- 6. Да се дефинира функция, която намира сбора на естествените числа в интервала, определен от двата параметъра.
intervalSum :: Int -> Int -> Int
intervalSum a b
  | a > b = 0
  | otherwise = a + intervalSum (a + 1) b

-- tail recursion version
intervalSumTail :: Int -> Int -> Int
intervalSumTail a b = helper a 0
  where
    helper :: Int -> Int -> Int
    helper i sum
      | i > b = sum
      | otherwise = helper (i + 1) (sum + i)

-- 7. Да се дефинират функции, които намират факториел и двоен факториел от подадено като параметър естествено число.
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

-- tail recursion
factTail :: Int -> Int
factTail n = helper 1 1
  where
    helper :: Int -> Int -> Int
    helper i pr
      | i > n = pr
      | otherwise = helper (i + 1) (i * pr)

-- 8. Да се дефинират функции, които намират най-големия общ делител и най-малкото общо кратно на две естествени числа.
gcd :: Int -> Int -> Int
gcd 0 0 = error "numbers cannot both be zero"
gcd a 0 = abs a
gcd 0 b = abs b
gcd a b
  | a > b = gcd b (a `mod` b)
  | otherwise = gcd a (b `mod` a)

lcm :: Int -> Int -> Int
lcm a b = abs (a * b) `div` gcd a b

-- 9. Да се дефинира функция, която проверява дали две цели числа са взаимно прости.
-- Чрез нея да се дефинира функцията на Ойлер,
-- която приема естествено число n и пресмята броя взаимно прости с n числа в интервала [ 1 , n − 1 ] .
coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

euler :: Int -> Int
euler n = helper 1 0
  where
    helper :: Int -> Int -> Int
    helper i count
      | i >= n = count
      | coprime i n = helper (i + 1) (count + 1)
      | otherwise = helper (i + 1) count

-- 10. Да се напише функция, която приема положително четно число, по-голямо от 2,
-- и връща двойка прости числа, чиято сума е равна на даденото число.
prime :: Int -> Bool
prime n
  | n < 2 = False
  | otherwise = helper 2
  where
    helper :: Int -> Bool
    helper i
      | i * i > n = True
      | n `mod` i == 0 = False
      | otherwise = helper (i + 1)

sumOfPrimeNumbers :: Int -> (Int, Int)
sumOfPrimeNumbers n = helper 2
  where
    helper :: Int -> (Int, Int)
    helper i
      | prime i && prime (n - i) = (i, n - i)
      | otherwise = helper (i + 1)
