
module HW04 where

import BST

---- We do not know the relationship between a and b.
---- So could not make b uaing a
---- The only possible function is identity function
--ex1 :: a -> b -> b
--ex1 x y = y 
--
--
--ex2 :: a -> a -> a
--ex2 a1 a2 = a1
--ex2 a1 a2 = a2
--
---- same as ex1
--ex3 :: Int -> a -> a
--ex3 int a1 = a1
--
---- We can make 4function
---- we have two input and can choice two output
---- so 2 * 2 = 4
--ex4 :: Bool -> a -> a -> a
--ex4 cond a1 a2 = a1
--
---- Same as ex4.
---- two input and two output
--ex5 :: Bool -> Bool
--ex5 _ = True
--
---- we have function which return a, but we does not have a to inpit
--ex6 :: (a -> a) -> a
--ex6 = error "impossible"
--
---- Two possible function
---- one is apply function to second argument
---- other one is ex1
--ex7 :: (a -> a) -> a -> a
--ex7 fun a1 = fun a1
--
---- Two possible function
---- one is identity function
---- other is always return empty list.
--ex8 :: [a] -> [a]
--ex8 as = as
--
---- there two possible function.
---- one is that map function to all as.
---- other is return empty list always
--ex9 :: (a -> b) -> [a] -> [b]
--ex9 fun as = map fun as
--
---- cannot remove maybe 
--ex10 :: Maybe a -> a
--ex10 = error "impossible"
--
---- there are two function.
---- one is return Just a
---- other is return Nothing
--ex11 :: a -> Maybe a
--ex11 a = Just a
--
---- there are two input and two output.
---- but Nothing -> Just a is impossible.
---- So 3 possible fiunctions exist.
--ex12 :: Maybe a -> Maybe a
--ex12 maybea = Nothing

insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ element (Leaf) = Node Leaf element Leaf
