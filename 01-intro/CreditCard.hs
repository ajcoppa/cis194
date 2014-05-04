module CreditCard where

-- | Convert an Integer to a list of its digits.
--
-- >>> toDigits 1234
-- [1,2,3,4]
-- >>> toDigits 0
-- []
-- >>> toDigits (-17)
-- []
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

-- | Convert an Integer to a reversed list of its digits.
--
-- >>> toDigitsRev 1234
-- [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- | Double every other integer in the list, starting from the right.
--
-- >>> doubleEveryOther [8,7,6,5]
-- [16,7,12,5]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = helper xs doubleFirstElement
  where doubleFirstElement = (even (length xs))
        helper [] _  = []
        helper (x:xs) True = x * 2 : helper xs False
        helper (x:xs) False = x : helper xs True

-- | Sum up the digits of integers in a list.
--
-- >>> sumDigits [16,7,12,5]
-- 22
sumDigits :: [Integer] -> Integer
sumDigits = sum . map sum . map toDigits
