module Hanoi where

type Peg = String
type Move = (Peg, Peg)

type Disc = Integer
type PegState = (Peg, [Disc])
type BoardState = [PegState]

{- | Solve the Towers of Hanoi puzzle by returning a list of Moves that will
transfer a tower of N discs from the first to the second peg.

>>> hanoi 2 "a" "b" "c"
[("a","c"),("a","b"),("c","b")]

This implementation is a literal translation of the spec, which says:

To move n discs (stacked in increasing size) from peg a to peg b using peg c
as temporary storage,
1. move n − 1 discs from a to c using b as temporary storage
2. move the top disc from a to b
3. move n − 1 discs from c to b using a as temporary storage.
-}
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a,b)] ++ hanoi (n - 1) c b a
