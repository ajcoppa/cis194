module CreditCard where

-- | Convert an Integer to a reversed list of its digits.
--
-- >>> toDigitsRev 1234
-- [4,3,2,1]
-- >>> toDigitsRev 0
-- []
-- >>> toDigitsRev (-17)
-- []
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- | Convert an Integer to a list of its digits.
--
-- >>> toDigits 1234
-- [1,2,3,4]
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

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

-- | Validates a credit card number.
--
-- >>> validate 4012888888881881
-- True
-- >>> validate 4012888888881882
-- False
validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
