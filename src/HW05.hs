module HW05 where

import Data.Bits
import Data.List
import Data.Maybe
import Ring

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

data Mat2x2 = MkMat Integer Integer Integer Integer
  deriving (Show, Eq)

instance Ring Mat2x2 where
  addId = MkMat 0 0 0 0
  addInv (MkMat a b c d) = MkMat (-a) (-b) (-c) (-d)
  mulId = MkMat 1 0 0 1

  add (MkMat a1 a2 a3 a4) (MkMat b1 b2 b3 b4) =
    MkMat (a1 + b1) (a2 + b2) (a3 + b3) (a4 + b4)
  mul (MkMat a1 a2 a3 a4) (MkMat b1 b2 b3 b4) =
    MkMat (a1 * b1 + a2 * b3) (a1 * b2 + a2 * b4)
          (a3 * b1 + a4 * b3) (a3 * b2 + a4 * b4)

instance Parsable Mat2x2 where
  parse inputStr = do restFromOpenParen <- stripPrefix "[[" inputStr
                      (firstElem, restFromFirstElem) <- parse restFromOpenParen

                      restFromFirstComma <- stripPrefix "," restFromFirstElem
                      (secondElem, restFromSecondElem) <- parse restFromFirstComma

                      restFromMiddleParen <- stripPrefix "][" restFromSecondElem
                      (thirdElem, restFromThirdElem) <- parse restFromMiddleParen

                      restFromSecondComma <- stripPrefix "," restFromThirdElem
                      (fourthElem, restFromFourthElem) <- parse restFromSecondComma

                      restFromAll <- stripPrefix "]]" restFromFourthElem
                      return (MkMat firstElem secondElem thirdElem fourthElem, restFromAll)

parseMatrix :: String -> Maybe (Mat2x2, String)
parseMatrix = parse

instance Ring Bool where
  addId = False
  addInv = not
  mulId = True

  add = xor
  mul = (&&)

instance Parsable Bool where
  parse = listToMaybe . reads
