ifEven f x = if even x
             then f x
             else x

genIfEven f = (\x -> ifEven f x)

--------------------------------------

genIfXEven x = (\f -> ifEven f x)

--------------------------------------

getRequestURL host apiKey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

exampleUrlBuilder = getRequestURL "http://example.com" "some_key" "book"

-------------------------------------

subtract2 = flip (-) 2

-------------------------------------

ifEvenInc x = ifEven ((+) 1) x
ifEvenDouble x = ifEven ((*) 2) x
ifEvenSquare x = ifEven (flip (^) 2) x

-------------------------------------

binaryPartialApplication binaryFunction =  (\x -> (\y -> binaryFunction x y))