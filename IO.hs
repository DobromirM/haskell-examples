import System.Random
import qualified Data.Map as Map

minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

main :: IO ()
main = do
  dieRoll <- randomRIO (minDie, maxDie)
  putStrLn (show dieRoll)

----------------------------------------------------

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

name :: Map.Map Int String
name = Map.fromList [(1, "Foo")]

maybeMain :: Maybe String
maybeMain = do
  name <- Map.lookup 1 name
  let statement = helloPerson name
  return statement

----------------------------------------------------

fastFib :: Int -> Int -> Int -> Int
fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y counter = fastFib y next (counter - 1)
  where next = x + y

fibMain :: IO ()
fibMain = do
  putStrLn "Please enter a number: "
  n <- getLine
  let fibN = fastFib 1 1 (read n)
  putStrLn (show fibN)
