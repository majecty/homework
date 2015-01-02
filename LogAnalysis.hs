{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Data.Char
import Data.List
import Log

parseTimestamp :: MessageType -> MaybeInt -> String -> String -> MaybeLogMessage
parseTimestamp messageType maybeTimestamp message fullString = 
  case maybeTimestamp of
    ValidInt timeStamp -> ValidLM $ LogMessage messageType timeStamp message
    InvalidInt -> InvalidLM fullString
  
parseMessageFromTokens :: [String] -> MaybeLogMessage
parseMessageFromTokens tokens@("E":numString:timeString:lefts) =
  case readInt numString  of
    ValidInt num -> parseTimestamp (Error num) (readInt timeString) (unwords lefts) (unwords tokens)
    InvalidInt -> InvalidLM $ unwords tokens 

parseMessageFromTokens tokens@("I":timeString:lefts) = parseTimestamp Info (readInt timeString) (unwords lefts) (unwords tokens)
parseMessageFromTokens tokens@("W":timeString:lefts) = parseTimestamp Warning (readInt timeString) (unwords lefts) (unwords tokens)
parseMessageFromTokens tokens = InvalidLM $ unwords tokens

parseMessage :: String -> MaybeLogMessage
parseMessage message = parseMessageFromTokens $ words message

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
-- validMessagesOnly maybeMessages = [logMessage | ValidLM logMessage <- maybeMessages]
  validMessagesOnly maybeMessages = do
    ValidLM logMessage <- maybeMessages
    return logMessage

parse :: String -> [LogMessage]
parse allText = validMessagesOnly $ map parseMessage $ lines allText

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ lhsTimeStamp _) (LogMessage _ rhsTimeStamp _) = compare lhsTimeStamp rhsTimeStamp

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages logMessages = sortBy compareMsgs logMessages

isSevereErrorMessages :: LogMessage -> Bool
isSevereErrorMessages (LogMessage (Error score) _ _) = score >= 50 
isSevereErrorMessages _ = False

getMessageBody :: LogMessage -> String
getMessageBody (LogMessage _ _ messageBody) = messageBody 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMessages =
  let errorMessages = [ errorMessage | errorMessage@(LogMessage (Error _) _ _) <- logMessages] in
  let severeErrorMessages = filter isSevereErrorMessages $ errorMessages in
  map getMessageBody $ sortMessages severeErrorMessages

lowercase :: String -> String
lowercase string = map toLower string

isMatchKeyword :: String -> LogMessage -> Bool
isMatchKeyword keyword (LogMessage _ _ messageBody) = isInfixOf (lowercase keyword) (lowercase messageBody)

messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout keyword logMessages = filter (isMatchKeyword keyword) logMessages

whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced keyword logMessages = whatWentWrong $ messagesAbout keyword logMessages
