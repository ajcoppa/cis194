{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad (replicateM)
import Control.Monad.Random
import Data.Function (on)
import Data.List
import Data.Monoid

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)
------------------------------------------------------------

{- | Given a battlefield with attackers and defenders, simulate a Risk battle
between them and give back a new battlefield with casualties applied.

The attacking player can send up to 3 attackers, but has to leave at least one
unit behind. The defending player can send up to 2 defenders.
-}
battle :: Battlefield -> Rand StdGen Battlefield
battle battlefield =
  let possibleAttackers = attackers battlefield - 1
      numAttackers = min 3 possibleAttackers
      numDefenders = min 2 (defenders battlefield)
  in
  if numAttackers < 1 || numDefenders < 1
  then return battlefield
  else do
    attackRolls <- replicateM numAttackers die
    defenseRolls <- replicateM numDefenders die
    let battleResult = compareRolls attackRolls defenseRolls
    let newBattlefield = applyBattleResult battleResult battlefield
    return newBattlefield

data BattleResult = BattleResult {
  attackerLosses :: Int,
  defenderLosses :: Int
} deriving (Show)

instance Monoid BattleResult where
  mempty = BattleResult {
    attackerLosses = 0,
    defenderLosses = 0
  }
  x `mappend` y = BattleResult {
    attackerLosses = attackerLosses x + attackerLosses y,
    defenderLosses = defenderLosses x + defenderLosses y
  }

applyBattleResult :: BattleResult -> Battlefield -> Battlefield
applyBattleResult battleResult battlefield =
  Battlefield {
    attackers = attackers battlefield - attackerLosses battleResult,
    defenders = defenders battlefield - defenderLosses battleResult
  }

{- $setup
>>> :{
  let mediocre = [DV 1, DV 2, DV 4]
      mediocre' = [DV 4, DV 2, DV 1]
      perfect = [DV 6, DV 6, DV 6]
      awful = [DV 1, DV 1, DV 1]
      extremes = [DV 6, DV 1, DV 1]
:}
-}

{- | Compares two sets of dice rolls to evaluate the results of a Risk battle.

Sort values in descending order, then compare pairs individually.

>>> compareRolls perfect awful
BattleResult {attackerLosses = 0, defenderLosses = 3}

>>> compareRolls mediocre awful
BattleResult {attackerLosses = 1, defenderLosses = 2}

>>> compareRolls mediocre perfect
BattleResult {attackerLosses = 3, defenderLosses = 0}

>>> compareRolls mediocre mediocre'
BattleResult {attackerLosses = 3, defenderLosses = 0}

>>> compareRolls extremes mediocre
BattleResult {attackerLosses = 2, defenderLosses = 1}
-}
compareRolls :: [DieValue] -> [DieValue] -> BattleResult
compareRolls attackValues defenseValues =
  let descAttackValues = sortByDescending $ map unDV attackValues
      descDefenseValues = sortByDescending $ map unDV defenseValues
      battlePairs = zip descAttackValues descDefenseValues
      battleResults = map compareValues battlePairs
      overallResult = mconcat battleResults
  in overallResult

{- | Compares an attack and defense value, returning a BattleResult
representing who took losses as a result of the battle. In the case of a tie,
the defender has the advantage.

>>> compareValues (3,2)
BattleResult {attackerLosses = 0, defenderLosses = 1}

>>> compareValues (1,6)
BattleResult {attackerLosses = 1, defenderLosses = 0}

>>> compareValues (3,3)
BattleResult {attackerLosses = 1, defenderLosses = 0}
-}
compareValues :: (Int, Int) -> BattleResult
compareValues (attackRoll, defenseRoll) =
  if attackRoll > defenseRoll
  then BattleResult {attackerLosses = 0, defenderLosses = 1}
  else BattleResult {attackerLosses = 1, defenderLosses = 0}

{- | Sorts a list in descending order.

>>> sortByDescending [1,3,2]
[3,2,1]
-}
sortByDescending :: Ord b => [b] -> [b]
sortByDescending = sortBy (flip compare)

invade :: Battlefield -> Rand StdGen Battlefield
invade battlefield =
  let battleOver :: Battlefield -> Bool
      battleOver bf = attackerWon bf || defenderWon bf
  in if battleOver battlefield
    then return battlefield
    else battle battlefield >>= invade

{- | Returns true if the battlefield has no defenders.

>>> attackerWon $ Battlefield {attackers = 4, defenders = 0}
True

>>> attackerWon $ Battlefield {attackers = 3, defenders = 1}
False
-}
attackerWon :: Battlefield -> Bool
attackerWon = (<= 0) . defenders

{- | Returns true if the battlefield has 1 attacker or fewer.

(Attackers have to leave 1 person behind, so they lose when they have 1
attacker remaining.)

>>> defenderWon $ Battlefield {attackers = 1, defenders = 4}
True

>>> defenderWon $ Battlefield {attackers = 3, defenders = 1}
False
-}
defenderWon :: Battlefield -> Bool
defenderWon = (<= 1) . attackers

successProb :: Battlefield -> Rand StdGen Double
successProb battlefield = do
  resultBattlefields <- replicateM 1000 (invade battlefield)
  return . (/ 1000.0) . fromIntegral . length . filter attackerWon
    $ resultBattlefields
