x :: Int
x = 2

y :: Integer
y = 2

letter :: Char
letter = 'a'

z :: Double
z = 0.5

isFun :: Bool
isFun = True

values :: [Int]
values = [1,2,3]

double :: Int -> Int
double n = n * 2

half :: Int -> Double
half n = (fromIntegral n) / 2

intHalf :: Integer -> Integer
intHalf n = n `div` 2

printDouble :: Int -> String
printDouble n = show (n * 2)

simple :: a -> a
simple x = x

myTail :: [a] -> [a]
myTail [] = []
myTail (_:xs) = xs

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x