
module HW08 where

import Text.Read
import Control.Monad.Random
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid

consumeABlock :: String -> Maybe String 
consumeABlock "" = Just ""
consumeABlock (firstDigit:others) = do
  num <- readMaybe [firstDigit]
  let aString = take num $ repeat 'a'
  remainString <- stripPrefix aString others
  return remainString

-- FIXME: Does not clear whether exit recurlsive.
consumeAllBlock :: String -> Maybe ()
consumeAllBlock "" = Just ()
consumeAllBlock input = do
  leftInput <- consumeABlock input
  consumeAllBlock leftInput

stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . consumeAllBlock

specialNumbers :: [Int]
specialNumbers = [x | x <- [1..], x `mod` 5 == 0, x `mod` 7 /= 0 ]

type StdRand = Rand StdGen

type Army = Int
data ArmyCounts = ArmyCounts { attackers :: Army, defenders :: Army }
 deriving (Show, Eq)
type DieRoll = Int

dieRoll :: StdRand DieRoll
dieRoll = liftRand $ randomR (1, 6)

instance Monoid ArmyCounts where
  mempty = ArmyCounts { attackers = 0, defenders = 0 }
  mappend lhs rhs = ArmyCounts {
    attackers = attackers lhs + attackers rhs,
    defenders = defenders lhs + defenders rhs
  }

battleResult :: DieRoll -> DieRoll -> ArmyCounts
battleResult attackerDie defenderDie = case compare attackerDie defenderDie of
  GT -> ArmyCounts { attackers = 0, defenders = -1 }
  _ -> ArmyCounts { attackers = -1, defenders = 0 }

battleResults :: [DieRoll] -> [DieRoll] -> ArmyCounts
battleResults attackerDies defenderDies = fold eachBattleResults
  where eachBattleResults = zipWith battleResult attackerDies defenderDies

battle :: ArmyCounts -> StdRand ArmyCounts
battle armyCounts = do
  rolledAttackerDies <- rollAttackerDies armyCounts
  rolledDefenderDies <- rollDefenderDies armyCounts
  return $ battleResults rolledAttackerDies rolledDefenderDies

invade :: ArmyCounts -> StdRand ArmyCounts
invade armyCounts
  | attackers armyCounts <= minAttackerCount = return armyCounts
  | defenders armyCounts <= 0 = return armyCounts
  | otherwise = battle armyCounts >>= invade

rollAttackerDies :: ArmyCounts -> StdRand [DieRoll]
rollAttackerDies armyCounts = do
  let ac = attackerCount armyCounts
  sequence $ take ac $ repeat dieRoll

rollDefenderDies :: ArmyCounts -> StdRand [DieRoll]
rollDefenderDies armyCounts = do
  let dc = defenderCount armyCounts
  sequence $ take dc $ repeat dieRoll

maxAttackerCount :: Army
maxAttackerCount = 3

maxDefenderCount :: Army
maxDefenderCount = 2

minAttackerCount :: Army
minAttackerCount = 1

attackerCount :: ArmyCounts -> Int
attackerCount (ArmyCounts { attackers = totalCount }) = min maxAttackerCount availableCount
  where availableCount = totalCount - 1

defenderCount :: ArmyCounts -> Int
defenderCount (ArmyCounts { defenders = totalCount }) = min maxDefenderCount totalCount

