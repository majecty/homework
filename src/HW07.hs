
module HW07 where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = fmap fib [0..]

fibs2 :: [Integer]
fibs2 = [0,1] ++ zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a leftStream) = a : streamToList leftStream

instance Show a => Show (Stream a) where
   show stream = show $ take 20 (streamToList stream)

streamRepeat :: a -> Stream a
streamRepeat element = Cons element (streamRepeat element)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap mapFunc (Cons firstA otherAs) = Cons (mapFunc firstA) (streamMap mapFunc otherAs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed translator firstSeed = (Cons firstSeed (streamFromSeed translator (translator firstSeed)))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons first firstLeftStream) secondStream = Cons first $ (interleaveStreams secondStream firstLeftStream)

zeros :: Stream Integer
zeros = streamRepeat 0

ones :: Stream Integer
ones = streamRepeat 1

ruler :: Stream Integer
ruler = interleaveStreams zeros $ (streamMap (+1) ruler)
