import System.Environment
import Control.Monad

mainArgs :: IO ()
mainArgs = do
  args <- getArgs
  let linesToRead = if length args > 0
                    then read (head args)
                    else 0 :: Int
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  print (sum ints)


myMain :: IO ()
myMain = do
  values <- mapM (\_ -> getLine) [1..3]
  mapM_ putStrLn values

myReplicateM :: Monad m => m a -> Integer -> m [a]
myReplicateM f n = mapM (\_ -> f) [1..n]

lazyMain :: IO ()
lazyMain = do
  userInput <- getContents
  let rev = reverse userInput
  putStrLn rev

sampleData :: String
sampleData = ['6', '2', '\n', '2', '1', '\n']

toInts :: String -> [Int]
toInts = map read . lines

numbersMain :: IO ()
numbersMain = do
  userInput <- getContents
  let numbers = toInts userInput
  print (sum numbers)

squaresMain :: IO ()
squaresMain = do
  userInput <- getContents
  let numbers = toInts userInput
  let squares = map (^2) numbers
  print (sum squares)


--------------------------------------------------------------------

doCalc :: Int -> Int -> String -> Int
doCalc a b "+" = a + b
doCalc a b "*" = a * b

toCalc :: [String] -> Int
toCalc (a : sign : b : _) = doCalc (read a) (read b) sign

calcMain :: IO ()
calcMain = do
  input <- getContents
  let result = toCalc (lines input)
  print result

--------------------------------------------------------------------

quotes :: [String]
quotes = ["Foo", "Bar", "Baz", "Qux", "Quux"]

findQuote :: [String] -> [String]
findQuote [] = []
findQuote ("n" : xs) = []
findQuote (x : xs) = quote : (findQuote xs)
  where quote = quotes !! (read x - 1)

quoteMain :: IO ()
quoteMain = do
  input <- getContents
  mapM_ putStrLn (findQuote (lines input))

main = quoteMain