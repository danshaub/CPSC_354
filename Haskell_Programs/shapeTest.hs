data Shape2D
  = Rectangle
      { length :: Float,
        width :: Float
      }
  | Circle
      { radius :: Float
      }
  | Triangle
      { sideA :: Float,
        sideB :: Float,
        angle :: Float
      }
  deriving (Show, Eq)

area :: Shape2D -> Float
area (Rectangle n m) = n * m
area (Circle r) = pi * r * r
area (Triangle m n p) = (m * n * (sin p)) / 2