module Main

data Matter = Solid
            | Liquid
            | Gas

Eq Matter where
  (==) Solid Solid = True
  (==) Liquid Liquid = True
  (==) Gas Gas = True
  (==) _ _ = False

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

Eq Shape where
  (==) (Triangle x z) (Triangle y w) = x == y && z == w
  (==) (Rectangle x z) (Rectangle y w) = x == y && z == w
  (==) (Circle x) (Circle y) = x == y
  (==) _ _ = False

area : Shape -> Double
area (Triangle h w) = h * w / 2
area (Rectangle h w) = h * w
area (Circle r) = pi * (pow r 2)

Ord Shape where
  min x y = case compare (area x) (area y) of
                 GT => x
                 LT => y
                 EQ => x

Show Shape where
  show (Triangle x y) = "Triangle " ++ show x ++ " " ++ show y
  show (Rectangle x y) = "Rectangle " ++ show x ++ " " ++ show y
  show (Circle x) = "Circle " ++ show x

