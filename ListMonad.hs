import Control.Monad
import Data.Char

powersOfTwo :: Int -> [Int]
powersOfTwo n = do
  value <- [1 .. n]
  return (2^value)

squares :: [(Int, Int)]
squares = do
  n <- [1 .. 10]
  return (n, n^2)

evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1 .. n]
  guard(even value)
  return value


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter func vals = do
  v <- vals
  guard (func v)
  return v


originalList :: [String]
originalList = ["brown","blue","pink","orange"]

listComp :: [String]
listComp = [mrColor
           | color <- originalList
           , let (first : rest) = color
           , let capitalised = (toUpper first) : rest
           , let mrColor = "Mr." ++ capitalised]

-------------------------------------------------------------------------------------------------

numDays :: [Int]
numDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

dates :: [Int]
dates = [days | n <- numDays, days <- [1 .. n]]

datesDo :: [Int]
datesDo = do
  n <- numDays
  days <- [1 .. n]
  return days

datesMonad :: [Int]
datesMonad = numDays >>= (\n -> [1 .. n] >>= (\days -> return days))