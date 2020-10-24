myGCD x y = if remainder == 0
            then y
            else myGCD y remainder
  where remainder = x `mod` y

------------------------------------------

sayAmount n = case n of
  1 -> "one"
  2 -> "two"
  n -> "a bunch"


------------------------------------------

sayAmount2 1 = "one"
sayAmount2 2 = "two"
sayAmount2 _ = "a bunch"

-----------------------------------------

myTail [] = []
myTail (_:xs) = xs

-----------------------------------------

myGCD2 x 0 = x
myGCD2 x y = myGCD2 y remainder
  where remainder = x `mod` y

-----------------------------------------

myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-----------------------------------------

myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x:rest
  where rest = myTake (n - 1) xs

----------------------------------------

myCycle xs = xs ++ myCycle xs

----------------------------------------

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

----------------------------------------

collatz 1 = 1
collatz n = if even n
            then 1 + collatz (n `div` 2)
            else 1 + collatz (n * 3 + 1)

----------------------------------------

myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

----------------------------------------

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

---------------------------------------

fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y counter = fastFib y next (counter - 1)
  where next = x + y
