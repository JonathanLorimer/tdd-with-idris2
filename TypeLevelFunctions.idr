module Main

import Data.Strings
import Data.Vect

AdderType : (numargs : Nat) -> Type
AdderType Z = Int
AdderType (S k) = (next : Int) -> AdderType k

adder : (numargs : Nat) -> (acc : Int) -> AdderType numargs
adder 0 acc = acc
adder (S k) acc = \next => adder k (next + acc)

data Format = Number Format
            | Str Format
            | Lit String Format
            | Character Format
            | NumDouble Format
            | End

PrintfType : Format -> Type
PrintfType (Number fmt) = (int : Int) -> PrintfType fmt
PrintfType (Str fmt) = (str : String) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType (Character fmt) = (char : Char) -> PrintfType fmt
PrintfType (NumDouble fmt) = (dbl : Double) -> PrintfType fmt
PrintfType End = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \int => printfFmt fmt (acc ++ show int)
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (NumDouble fmt) acc = \dbl => printfFmt fmt (acc ++ show dbl)
printfFmt (Character fmt) acc = \char => printfFmt fmt (acc ++ singleton char)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Character (toFormat chars)
toFormat ('%' :: 'f' :: chars) = NumDouble (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                             (Lit lit chars') => Lit (strCons c lit) chars'
                             fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt (toFormat (unpack fmt)) ""

Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Double)

testMatrix : Matrix 2 3
testMatrix = [[0,0,0],[0,0,0]]

TupleVect : Nat -> Type -> Type
TupleVect 0 ty = ()
TupleVect (S k) ty = (ty, TupleVect k ty)

test : TupleVect 4 Nat
test = (1, 2, 3, 4, ())

