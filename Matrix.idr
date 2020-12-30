module Main

import Data.Vect

matrix1 : Vect 3 (Vect 3 Int)
matrix1 = [[1  , 2  , 3]
          ,[6  , 7  , 8]
          ,[12 , 13 , 14]]

matrix2 : Vect 3 (Vect 3 Int)
matrix2 = [[2 , 3 , 4]
          ,[3 , 4 , 5]
          ,[4 , 5 , 6]]

addRow : Num numType =>
         Vect cols numType ->
         Vect cols numType ->
         Vect cols numType
addRow xs ys = zipWith (+) xs ys

addMatrix : Num numType =>
            Vect rows (Vect cols numType) ->
            Vect rows (Vect cols numType) ->
            Vect rows (Vect cols numType)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = addRow x y :: addMatrix xs ys


createEmpties : {n : Nat} -> Vect n (Vect 0 a)
createEmpties = replicate n []

transposeMatrix : {n : Nat} -> Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMatrix [] = createEmpties
transposeMatrix (x :: xs) = let xsTrans = transposeMatrix xs in
                                transHelper x xsTrans
  where
    transHelper : Vect s a ->
                  Vect s (Vect k a) ->
                  Vect s (Vect (S k) a)
    transHelper [] [] = []
    transHelper (x :: xs) (y :: ys) = (x :: y) :: transHelper xs ys

transposeMatrix' : {n : Nat} -> Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMatrix' [] = createEmpties
transposeMatrix' (x :: xs) = let xsTrans = transposeMatrix xs in
                                zipWith (::) x xsTrans

mulRow : Num numType => Vect n numType -> Vect p (Vect n numType) -> Vect p numType
mulRow _ [] = []
mulRow xs (y :: ys) =  sum (zipWith (*) xs y) :: mulRow xs ys

go : Num numType =>
     Vect m (Vect n numType) ->
     Vect p (Vect n numType) ->
     Vect m (Vect p numType)
go [] _ = []
go (x :: xs) ys = mulRow x ys :: go xs ys

mulMatrix : Num numType =>
            {p : Nat} ->
            Vect m (Vect n numType) ->
            Vect n (Vect p numType) ->
            Vect m (Vect p numType)
mulMatrix xs ys = let transYs = transposeMatrix' ys in
                      go xs transYs




