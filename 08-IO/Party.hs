{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Monoid
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
