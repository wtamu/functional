module Main where

import Lib

main :: IO ()
main = someFunc

-- EXERCISE 1
-- convert positive Integers to a list of digits.
-- Example: toDigits 1234 == [1,2,3,4]
-- Example: toDigits 0 == []
-- Example: toDigits (-17) == []
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

-- convert positive Integers to a reversed list of digits.
-- Example: toDigitsRev 1234 == [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

-- EXERCISE 2
-- Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- Example: doubleEveryOther [1,2,3] == [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther [x]      = [x]
doubleEveryOther xs       = doubleEveryOther rest ++ [y*2] ++ [z]
  where rest = init (init xs)
        y = last (init xs)
        z = last xs

-- EXERCISE 3
-- Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits []  = 0
sumDigits [x] = x
sumDigits xs = sum (toDigits (head xs)) + sumDigits (tail xs)

-- EXERCISE 4
-- Example: validate 4012888888881881 = True
-- Example: validate 4012888888881882 = False
-- toDigits, toDigitsRev, doubleEveryOther, sumDigits
validate :: Integer -> Bool
validate n = total `mod` 10 == 0
  where xs  = toDigits n
        dbl = doubleEveryOther xs
        total = sumDigits dbl


-- EXERCISE 5
-- Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
--type Peg = String
--type Move = (Peg, Peg)
--hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]


myDrop :: Int -> [a] -> [a]
myDrop n xs
  | n <= 0 = xs
  | otherwise = myDrop (n-1) (tail xs)

lastButOne :: [a] -> a
lastButOne (x:[]) = x
lastButOne xs = last (init xs)


fib :: Integer -> [Integer]
fib 1 = [1]
fib 2 = [2]
fib n = fib (n) + fib (n - 1)



