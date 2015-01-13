{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import Data.Aeson
-- import Data.Monoid
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
  deriving (Show, Generic, Eq)

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
