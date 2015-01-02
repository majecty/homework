{-
Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy (x:otherChars) hand
  | elem x hand = formableBy otherChars $ delete x hand
  | otherwise = False

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

compareTemplateCharacter :: (Char, Char) -> Bool
compareTemplateCharacter (tempChar, wordChar)
  | tempChar == '?' = True
  | otherwise = tempChar == wordChar

matchTemplate :: Template -> String -> Bool
matchTemplate template word = and $ map compareTemplateCharacter zipped
  where zipped = zip template word
  
getNeededCharacters :: Template -> String -> [Char]
getNeededCharacters template word = map snd $ filter ((==) '?' . fst) $ zipped
  where zipped = zip template word

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate template hand word
  | length template /= length word = False
  | matchTemplate template word == False = False
  | otherwise = formableBy neededCharacters hand
  where neededCharacters = getNeededCharacters template word
  
wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate template hand = filter (wordFitsTemplate template hand) allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord word = sum $ map scrabbleValue word

bestWords :: [String] -> [String]
bestWords words =
  let scores = map scrabbleValueWord words in
  let bestScore = maximum scores in
  let wordsWithScore = zip words scores in
  map fst $ filter ((==) bestScore . snd) $ wordsWithScore
  
getScoreWithSpecialCharacter :: Char -> Int -> Int
getScoreWithSpecialCharacter specialCharacter baseScore
  | specialCharacter == 'D' = 2 * baseScore
  | specialCharacter == 'T' = 3 * baseScore
  | otherwise = baseScore
  

wordBonus :: Char -> Int
wordBonus '2' = 2
wordBonus '3' = 3
wordBonus _ = 1

getWordBonus :: STemplate -> Int
getWordBonus stemplate = product $ map wordBonus $ stemplate


scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate template word =
  let baseScores = map scrabbleValue word in
  let zipped = zip template baseScores in
  let wordBonus = getWordBonus template in
  (*) wordBonus $ sum $ map (uncurry getScoreWithSpecialCharacter) zipped


test :: String
test = show $
  (formableBy "cute" "utecab" == True) :
  (formableBy "cute" "vrute" == False) :
  (wordFitsTemplate "??t?" "cuesw" "cute" == True) :
  (scrabbleValueWord "ab" == 4) :
  (bestWords ["cat", "rat", "bat"] == ["cat", "bat"]) :
  (scrabbleValueTemplate "DD" "ab" == 8) :
  (scrabbleValueTemplate "2D" "ab" == 14) :
  []

main :: IO ()
main = print test
