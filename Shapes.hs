data Shape = Circle Radius | Square Width | Rectangle Width Height
  deriving Show

perimeter :: Shape -> Double
perimeter (Circle r) = r * pi * 2
perimeter (Square n) = 4 * n
perimeter (Rectangle m n) = 2 * (m + n)

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Square n) = n * n
area (Rectangle m n) = m * n

type Radius = Double
type Width = Double
type Height = Double
