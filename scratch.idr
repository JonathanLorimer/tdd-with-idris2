module Main

import Data.List
import Data.Strings

data Bin : Type where
  On  : Bin
  Off : Bin

flipBin : Bin -> Bin
flipBin On = Off
flipBin Off = On

palindrome : Nat -> String -> Bool
palindrome i x = let xs = toLower x
                in if length x > i
                      then xs == reverse xs
                      else False

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)


isPalindrome : Nat -> String -> Bool
isPalindrome = palindrome

allLengths : List String -> List Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs

mutual
  isEven : Nat -> Bool
  isEven 0 = True
  isEven (S k) = isOdd k

  isOdd : Nat -> Bool
  isOdd 0 = False
  isOdd (S k) = isEven k


