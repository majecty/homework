{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import Data.Aeson
import Data.Maybe
import Data.Monoid
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text                  as T
-- import qualified Data.Text.IO               as T

ynToBool :: Value -> Value
ynToBool (Object obj) = Object $ fmap ynToBool obj
ynToBool (Array array) = Array $ fmap ynToBool array
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool other = other

parseData :: B.ByteString -> Either String Value
parseData input = fmap ynToBool $ eitherDecode input

data Market = Market { marketname :: T.Text
                    ,  x :: Float
                    ,  y :: Float
                    ,  state :: T.Text }
  deriving (Show, Generic)

instance FromJSON Market

resultToEither :: Result a -> Either String a
resultToEither (Error errorMsg) = Left errorMsg
resultToEither (Success a) = Right a

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets input = do
  parsedValue <- parseData input
  markets <- resultToEither $ fromJSON parsedValue
  return markets

loadData :: IO [Market]
loadData = do
  filedata <- B.readFile "markets.json"
  let parseResult = parseMarkets filedata
  either fail return parseResult

data OrdList a = OrdList { getOrdList :: [a] }
   deriving (Eq, Show)

mergeTwoListWithOrder :: Ord a => [a] -> [a] -> [a]
mergeTwoListWithOrder xs [] = xs
mergeTwoListWithOrder [] ys = ys
mergeTwoListWithOrder xs@(x:xLefts) ys@(y:yLefts) = case compare x y of
  LT -> x:(mergeTwoListWithOrder xLefts ys)
  EQ -> x:(mergeTwoListWithOrder xLefts ys)
  GT -> y:(mergeTwoListWithOrder xs yLefts)

instance Ord a => Monoid (OrdList a) where
  mempty = OrdList []
  mappend (OrdList lhs) (OrdList rhs) = OrdList $ mergeTwoListWithOrder lhs rhs

 -- you’ll need to fill in the ellipses
evens :: OrdList Integer
evens = OrdList [2,4,6]
odds :: OrdList Integer
odds = OrdList [1,3,5]
combined :: OrdList Integer
combined = evens <> odds

type Searcher m = T.Text -> [Market] -> m
search :: Monoid m => (Market -> m) -> Searcher m
search _ _ [] = mempty
search resultMaker searchInput (fstMarket@(Market { marketname = name }):otherMarkets) =
  case T.isInfixOf searchInput name of
    True -> mappend (resultMaker fstMarket) (search resultMaker searchInput otherMarkets)
    False -> mempty

compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.) . (.)

firstFound :: Searcher (Maybe Market)
firstFound = compose2 listToMaybe (search (:[]))

lastListToMaybe :: [a] -> Maybe a
lastListToMaybe [] = Nothing
lastListToMaybe xs = Just (last xs)

lastFound :: Searcher (Maybe Market)
lastFound = compose2 lastListToMaybe (search (:[]))

allFound :: Searcher [Market]
allFound = search (:[])
