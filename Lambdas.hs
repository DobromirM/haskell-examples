-- With where clause
sumSquareOrSquareSum x y = if sumSquare > squareSum
                         then sumSquare
                         else squareSum
  where sumSquare = x^2 + y^2
        squareSum = (x + y) ^ 2

-- With intermediate function
body sumSquare squareSum = if sumSquare > squareSum
                           then sumSquare
                           else squareSum

sumSquareOrSquareSum2 x y = body (x ^ 2 + y ^ 2) ((x + y) ^ 2)

-- With lambda
sumSquareOrSquareSum3 x y = (\sumSquare squareSum ->
                            if sumSquare > squareSum
                            then sumSquare
                            else squareSum) (x ^ 2 + y ^ 2) ((x + y) ^ 2)

-- With let
sumSquareOrSquareSum4 x y = let sumSquare = (x^2 + y^2)
                                squareSum = (x + y) ^ 2
                            in
                             if sumSquare > squareSum
                               then sumSquare
                               else squareSum


-- With where clause
doubleDouble x = dubs * 2
  where dubs = x * 2

-- With lambda

doubleDouble2 x = (\dubs -> dubs * 2) (x * 2)

-- Overwriting

overwrite x = let x = 2
              in
                let x = 3
                in
                  let x = 4
                  in
                    x

overwrite2 x = (\x -> (\x -> (\x -> x) 4 ) 3 ) 2

x = 4
add1 y = y + x
add2 y = (\x -> y + x) 3
add3 y = (\y ->
          (\x -> y + x) 1 ) 2


counter x = (\x -> x + 1)
            ((\x -> x + 1)
            ((\x -> x) x ))