
{- Checksum
	1. Double the value of every second digit beginning from the right.
	2. Add the digits of the doubled values and the undoubled digits from the original number.
	3. Calculate the remainder when the sum is divided by 10.
	4. If the result equals 0, then the number is valid.

	author: wtamura
	date: 3/1/2018
-}

-- Converts positive Integers to a list of digits.
-- Example: toDigits 1234 == [1,2,3,4]
-- Example: toDigits 0 == []
-- Example: toDigits (-17) == []
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]


-- Converts positive Integers to a list of digits and reverses them.
-- Example: toDigitsRev 1234 == [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = [n `mod` 10] : toDigitsRev (n `div` 10)


x = 4