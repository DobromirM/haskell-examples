import Data.List

line :: Int -> String -> String -> String -> String
line n start middle end = start ++ m ++ end
  where m = concat (replicate n middle)

topLine :: Int -> String
topLine n = line n "┌" "────" "┐"

middleLine :: Int -> String
middleLine n = line n "│" "    " "│"

bottomLine :: Int -> String
bottomLine n = line n "└" "────" "┘"

square :: Int -> [String]
square n = topLine n : middle ++ [bottomLine n]
  where middle = replicate n (middleLine n)

concentricSquares :: Int -> String
concentricSquares n = intercalate "\n" merged
  where size = n * 2
        squares = map square [2, 4 .. size]
        merged = foldl1 mergeSquares squares

mergeSquares :: [String] -> [String] -> [String]
mergeSquares x (y:ys) = y : zipWith mergeLines x ys ++ [last ys]

mergeLines :: String -> String -> String
mergeLines x y = [head y] ++ "   " ++ x ++ "   " ++ [last y]

main :: IO ()
main =
  putStrLn "Enter square number:" >>
  readLn >>= putStrLn . concentricSquares
