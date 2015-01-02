{-
Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW01 where         -- We'll learn more about this later

isThisWorking :: String
isThisWorking = "Yes"
-- Load this file into GHCi (say, with `ghci HW01.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

-- Put your work below.

lastDigit :: Integer -> Integer
-- lastDigit 0 = 0
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `quot` 10

toDigits :: Integer -> [ Integer ]
toDigits n
  | n <= 0 = [ ]
  | otherwise = toDigits (dropLastDigit n) ++ [ lastDigit n ]

doubleEveryOtherFromLeft :: [ Integer ] -> [ Integer ]
doubleEveryOtherFromLeft (x:(y:xz)) =
  x:(2 * y):(doubleEveryOtherFromLeft xz)
doubleEveryOtherFromLeft x = x

doubleEveryOther :: [ Integer ] -> [ Integer ]
doubleEveryOther xz = reverse . doubleEveryOtherFromLeft . reverse $ xz

sumDigits :: [ Integer ] -> Integer
sumDigits xz = 
  let digits = concat $ map toDigits xz in
  sum digits
  
validate :: Integer -> Bool
validate n = remainder == 0 where
  remainder = lastDigit . sumDigits . doubleEveryOther . toDigits $ n

test :: String
test = show $
--  and $
   (lastDigit 12 == 2) :
   (lastDigit 4632 == 2) :
   (lastDigit 0 == 0) :
   (dropLastDigit 243 == 24) :
   (dropLastDigit 5 == 0) :
   (toDigits 123 == [1, 2, 3]) :
   (toDigits (-1) == []) :
   (doubleEveryOther [8, 7, 6, 5] == [16, 7, 12, 5]) :
   (doubleEveryOther [1, 2, 3] == [1, 4, 3]) :
   (sumDigits [1, 2, 5] == 8) :
   (sumDigits [16, 7, 12, 5] == 22) :
   (validate 4012888888881881 == True) :
   (validate 4012888888881882 == False) :
   (hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]) :
   []

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [ Move ]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

main :: IO ()
--main = print $ hanoi 1 "a" "b" "c"
main = print test
--lastDigit 5 == 5
--lastDigit 241 == 1






