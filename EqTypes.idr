module Main

import Data.Nat
import Data.List
import Data.Vect
import Decidable.Equality

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat 0 0 = Just Refl
checkEqNat 0 (S k) = Nothing
checkEqNat (S k) 0 = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              Just prf => Just (cong S prf)


same_cons : {xs : List a} -> {ys : List a} ->
            xs = ys -> x :: xs = x :: ys
same_cons prf = cong (x ::) prf

same_cons' : {xs : List a} -> {ys : List a} ->
            xs = ys -> x :: xs = x :: ys
same_cons' Refl = Refl

same_lists : {xs : List a} -> {ys : List a} ->
             x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl Refl = Refl

data ThreeEq : a -> b -> c -> Type where
  AllSame : ThreeEq x x x

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS x x x AllSame = AllSame


myReverse : {n : Nat} -> Vect n elem -> Vect n elem
myReverse [] = []
myReverse (x :: xs) = reverseProof (myReverse xs ++ [x])
  where
    reverseProof : Vect (k + 1) elem -> Vect (S k) elem
    reverseProof {k} result = rewrite plusCommutative 1 k in result

append_nil : Vect m elem -> Vect (plus m 0) elem
append_nil xs = rewrite plusZeroRightNeutral m in xs

append_xs : Vect (S (m + k)) elem -> Vect (m + (S k)) elem
append_xs xs = rewrite sym (plusSuccRightSucc m k) in xs

append : Vect n elem -> Vect m elem -> Vect (m + n) elem
append [] ys = append_nil ys
append (x :: xs) ys = append_xs (x :: append xs ys)

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes 0 m = sym (plusZeroRightNeutral m)
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in
                                 plusSuccRightSucc m k

reverseProof_xs : Vect (S n + k) a -> Vect (plus n (S k)) a
reverseProof_xs {n} {k} xs = rewrite sym (plusSuccRightSucc n k) in xs

reverseProof_nil : Vect n a -> Vect (n + 0) a
reverseProof_nil {n} xs = rewrite plusZeroRightNeutral n in xs



myReverse' : Vect n a -> Vect n a
myReverse' xs = rev [] xs
  where rev : Vect m a -> Vect k a -> Vect (m + k) a
        rev acc [] = reverseProof_nil acc
        rev acc (y :: ys) = reverseProof_xs (rev (y :: acc) ys)

twoPlusTwoNotFive : 2 + 2 = 5 -> Void
twoPlusTwoNotFive Refl impossible

zeroNotSuc : 0 = S k -> Void
zeroNotSuc Refl impossible

sucNotZero : S k = 0 -> Void
sucNotZero Refl impossible

noRec : (k = j -> Void) -> S k = S j -> Void
noRec contra prf = contra (cong (\(S a) => a) prf)

checkEqNat' : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat' 0 0 = Yes Refl
checkEqNat' 0 (S k) = No zeroNotSuc
checkEqNat' (S k) 0 = No sucNotZero
checkEqNat' (S k) (S j) = case checkEqNat' k j of
                               (Yes prf) => Yes (cong S prf)
                               (No contra) => No (noRec contra)

exactLength : {m : _} -> (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case decEq m len of
                                 (Yes Refl) => Just input
                                 (No contra) => Nothing

headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
                         (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
                         (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl

