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
