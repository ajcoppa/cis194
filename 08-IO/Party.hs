{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Monoid
import Data.Tree
import Employee

-- | Adds an Employee to the GuestList, updating the cached Fun score
--
-- >>> glCons (Emp "Sue" 5) (GL [(Emp "David" 1)] 1)
-- GL [Emp {empName = "Sue", empFun = 5},Emp {empName = "David", empFun = 1}] 6
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

-- | Monoid instance for GuestList.
--
-- >>> mempty :: GuestList
-- GL [] 0
-- >>> mappend (GL [(Emp "David" 1)] 1) (GL [(Emp "Nicole" 2)] 2)
-- GL [Emp {empName = "David", empFun = 1},Emp {empName = "Nicole", empFun = 2}] 3
-- >>> mempty <> (GL [Emp "Bobby" 38] 38)
-- GL [Emp {empName = "Bobby", empFun = 38}] 38
instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL xs f1) (GL ys f2) = GL (xs ++ ys) (f1 + f2)

-- |  Returns whichever input GuestList has the higher fun score.
--
-- >>> moreFun (GL [(Emp "David" 1)] 1) (GL [(Emp "Nicole" 2)] 2)
-- GL [Emp {empName = "Nicole", empFun = 2}] 2
-- >>> moreFun (GL [(Emp "Charlie" 7)] 7) (GL [(Emp "Phoebe" 20)] 20)
-- GL [Emp {empName = "Phoebe", empFun = 20}] 20
-- >>> moreFun (GL [(Emp "Joaquin" 7)] 7) (GL [(Emp "Stan" 7)] 7)
-- GL [Emp {empName = "Joaquin", empFun = 7}] 7
moreFun :: GuestList -> GuestList -> GuestList
moreFun x y = if compare x y == LT then y else x

-- | Folds over a rose tree.
--
-- >>> treeFold (\x xs -> x + sum xs) Node {rootLabel = 1, subForest = [Node {rootLabel = 7, subForest = []}]}
-- 8
-- >>> treeFold (\_ xs -> 1 + length xs) Node {rootLabel = 1, subForest = [Node {rootLabel = 7, subForest = []}, Node {rootLabel = 33, subForest = []}]}
-- 3
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f t =
  let subResult = map (treeFold f) (subForest t)
  in f (rootLabel t) subResult

{- $setup
>>> :{
let korey = Emp "Korey" 100
    bill = Emp "Bill" 5
    wenjie = Emp "Wenjie" 3
    kevin = Emp "Kevin" 5
    bulman = Emp "Bulman" 6
    hartsock = Emp "Hartsock" 38
    pat = Emp "Pat" 500
    alaina = Emp "Alaina" 40
    carly = Emp "Carly" 100
    zoe = Emp "Zoe" 50
    rick = Emp "Rick" 6
    mikah = Emp "Mikah" 38
    spencer = Emp "Spencer" 100
    sampson = Emp "Sampson" 3
:}
-}

{- | Given a boss and a list of possible guest lists for that boss's
employees, computes both the best guest list that includes the boss and the
best guest list that doesn’t include the boss.

These first examples are built from this tree:
                    Bulman 6
          Kevin 5 < Hartsock 38
Pat 500 <
          Bill 5  < Korey 100
                    Wenjie 3

>>> nextLevel korey []
(GL [Emp {empName = "Korey", empFun = 100}] 100,GL [] 0)

>>> :{
nextLevel bill [(GL [korey] 100,GL [] 0), (GL [wenjie] 3,GL [] 0)]
:}
(GL [Emp {empName = "Bill", empFun = 5}] 5,GL [Emp {empName = "Korey", empFun = 100},Emp {empName = "Wenjie", empFun = 3}] 103)

>>> :{
nextLevel kevin [(GL [bulman] 6,GL [] 0), (GL [hartsock] 38, GL [] 0)]
:}
(GL [Emp {empName = "Kevin", empFun = 5}] 5,GL [Emp {empName = "Bulman", empFun = 6},Emp {empName = "Hartsock", empFun = 38}] 44)

>>> :{
nextLevel pat [
  (GL [kevin] 5, GL [bulman, hartsock] 44)
, (GL [bill] 5, GL [korey, wenjie] 103)
]
:}
(GL [Emp {empName = "Pat", empFun = 500},Emp {empName = "Bulman", empFun = 6},Emp {empName = "Hartsock", empFun = 38},Emp {empName = "Korey", empFun = 100},Emp {empName = "Wenjie", empFun = 3}] 647,GL [Emp {empName = "Bulman", empFun = 6},Emp {empName = "Hartsock", empFun = 38},Emp {empName = "Korey", empFun = 100},Emp {empName = "Wenjie", empFun = 3}] 147)

Then another, based on this tree:
                        Rick 6
            Carly 100 < Mikah 38
Alaina 40 <
            Zoe 50    < Spencer 100
                        Sampson 3

>>> :{
nextLevel alaina [
  (GL [carly] 100, GL [rick,mikah] 44)
, (GL [zoe] 50, GL [spencer,sampson] 103)
]
:}
(GL [Emp {empName = "Alaina", empFun = 40},Emp {empName = "Rick", empFun = 6},Emp {empName = "Mikah", empFun = 38},Emp {empName = "Spencer", empFun = 100},Emp {empName = "Sampson", empFun = 3}] 187,GL [Emp {empName = "Carly", empFun = 100},Emp {empName = "Spencer", empFun = 100},Emp {empName = "Sampson", empFun = 3}] 203)
-}
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel me glPairs =
  let bestWithoutDirects = map snd glPairs
      bestWithSkips = mconcat bestWithoutDirects
      bestWithSkipsAndMe = glCons me bestWithSkips
      pickTheBest :: (GuestList, GuestList) -> GuestList
      pickTheBest pair = moreFun (fst pair) (snd pair)
      bestLists = map pickTheBest glPairs
      bestWithoutMe = mconcat bestLists
  in (bestWithSkipsAndMe, bestWithoutMe)

{- | Given an employee tree, find the GuestList that maximizes fun.

>>> maxFun testCompany
GL [Emp {empName = "John", empFun = 1},Emp {empName = "Sue", empFun = 5},Emp {empName = "Fred", empFun = 3},Emp {empName = "Sarah", empFun = 17}] 26
-}
maxFun :: Tree Employee -> GuestList
maxFun t =
  let topTwo = treeFold nextLevel t
      one = fst topTwo
      two = snd topTwo
  in moreFun one two

{- | Load the organization in company.txt and print the maximum possible fun
along with the guest list that produces it.
-}
main :: IO ()
main = do
  orgString <- readFile "company.txt"
  let org = read orgString
      guestList = maxFun org
      fun = getFun guestList
      employees = getEmployees guestList
  putStrLn $ "Total fun: " ++ show fun
  mapM_ (putStrLn . getName) employees

{- | Get a GuestList's fun.
>>> getFun (GL [] 8)
8
-}
getFun :: GuestList -> Fun
getFun (GL _ f) = f

{- | Get a GuestList's employees.
>>> getEmployees (GL [] 8)
[]
-}
getEmployees :: GuestList -> [Employee]
getEmployees (GL es _) = es

{- | Get an Employee's name.
>>> getName (Emp "George" 15)
"George"
-}
getName :: Employee -> Name
getName (Emp n _) = n
