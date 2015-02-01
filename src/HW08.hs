
module HW08 where

import Text.Read
import Data.List
import Data.Maybe

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
