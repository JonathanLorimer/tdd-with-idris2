module Main

import Data.Vect

fourInts : Vect 4 Int
fourInts = [0,1,2,3]

sixInts : Vect 6 Int
sixInts = [4,5,6,7,8,9]

tenInts : Vect 10 Int
tenInts = fourInts ++ sixInts

-- total allLengths : Vect len String -> Vect len Nat
-- allLengths [] = []
-- allLengths (x :: xs) = length x :: allLengths xs

allLengths : Vect len String -> Vect len Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs

insert : Ord elem => elem -> Vect len elem -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          True => y :: insert x xs
                          False => x :: y :: xs

insSort : Ord elem => Vect len elem -> Vect len elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                        insert x xsSorted

length' : List a -> Nat
length' [] = 0
length' (x :: xs) = 1 + (length' xs)

reverse : List a -> List a
reverse [] = []
reverse (x :: xs) = reverse xs ++ [x]

mapL : (a -> b) -> List a -> List b
mapL f [] = []
mapL f (x :: xs) = f x :: mapL f xs

mapV : (a -> b) -> Vect n a -> Vect n b
mapV f [] = []
mapV f (x :: xs) = f x :: mapV f xs

