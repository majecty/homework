{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import Data.Aeson
-- import Data.Monoid
-- import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
-- import qualified Data.Text                  as T
-- import qualified Data.Text.IO               as T

ynToBool :: Value -> Value
ynToBool (Object obj) = Object $ fmap ynToBool obj
ynToBool (Array array) = Array $ fmap ynToBool array
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool other = other

parseData :: B.ByteString -> Either String Value
parseData input = fmap ynToBool $ eitherDecode input
