module HW05 where

import Ring
import Parser

data Mod5 = MkMod Integer
   deriving (Show, Eq)

intToMod5 :: Integer -> Mod5
intToMod5 int = MkMod (int `mod` 5)

instance Ring Mod5 where
  addId = MkMod 0
  addInv (MkMod 0) = MkMod 0
  addInv (MkMod x) = MkMod $ 5 - x
  mulId = MkMod 1

  add (MkMod lhs) (MkMod rhs) = MkMod $ (lhs + rhs) `mod` 5
  mul (MkMod lhs) (MkMod rhs) = MkMod $ (lhs * rhs) `mod` 5

instance Parsable Mod5 where
  parse inputString
    | Just (int, left) <- parseInt inputString = Just ((intToMod5 int), left)
    | otherwise = Nothing
    where parseInt = (parse :: String -> Maybe (Integer, String))
