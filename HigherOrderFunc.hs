import Data.Char (toLower)

----------------------------------------

addAnA [] = []
addAnA (x:xs) = ("a " ++ x) : addAnA xs

----------------------------------------

squareAll [] = []
squareAll (x:xs) = x^2 : squareAll xs

----------------------------------------

myFilter test [] = []
myFilter test (x:xs) = if test x
                       then x : myFilter test xs
                       else myFilter test xs

---------------------------------------

myRemove test [] = []
myRemove test (x:xs) = if test x
                       then myRemove test xs
                       else x : myRemove test xs


---------------------------------------

myProduct xs = foldl (*) 1 xs

---------------------------------------

concatAll xs = foldl (++) "" xs

---------------------------------------

sumOfSquares xs = foldl (+) 0 (map (^2) xs)

---------------------------------------

rcons x y = y : x
myReverse xs = foldl rcons [] xs

---------------------------------------

myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f current xs
  where current = f init x

---------------------------------------

myElem elem xs = length matching > 0
  where matching = filter (elem == ) xs

---------------------------------------


isPalindrome xs = lower == reverse lower
  where noSpaces = filter (/= ' ')  xs
        lower = map toLower noSpaces

---------------------------------------

harmonic n = sum series
  where series = map (1 / ) [1..n]