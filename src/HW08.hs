
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
