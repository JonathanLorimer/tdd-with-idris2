module Main

data Expr : Type where
  Number : Int -> Expr
  Addition : Expr -> Expr -> Expr
  Subtraction : Expr -> Expr -> Expr
  Multiplication : Expr -> Expr -> Expr

evaluate : Expr -> Int
evaluate (Number x) = x
evaluate (Addition x y) = evaluate x + evaluate y
evaluate (Subtraction x y) = evaluate x - evaluate y
evaluate (Multiplication x y) = evaluate x * evaluate y

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing x = x
maxMaybe x Nothing = x
maxMaybe (Just x) (Just y)
  = case x `compare` y of
      LT => Just x
      EQ => Just y
      GT => Just y




