test :: IO String
test =  pure "Hello World"

doorPrize :: [Int]
doorPrize = [1000,2000,3000]

boxPrize :: [Int]
boxPrize = [10,50]

primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite allNums
  where allNums = [2 .. n]
        composite = (*) <$> allNums <*> allNums
        isNotComposite = not . (`elem` composite)

------------------------------------------------------------------------

data User = User {
  name :: String,
  gamerId :: Int,
  score :: Int
} deriving Show

testNames :: [String]
testNames = ["John Smith"
            ,"Robert'); DROP TABLE Students;--"
            ,"Christina NULL"
            ,"Randall Munroe"]

testIds :: [Int]
testIds = [1337
          ,0123
          ,999999]

testScores :: [Int]
testScores = [0
             ,100000
             ,-99999]

testData :: [User]
testData = pure User <*> testNames <*> testIds <*> testScores

--------------------------------------------------------------------------

allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap func val = (pure func) <*> val

example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> (pure (+) <*> pure 2 <*> pure 4) <*> pure 6

-------------------------------------------------------------------------

pack :: [Int]
pack = [6, 12]

drank :: Int
drank = 4

people :: [Int]
people = [4, 5]

willDrink :: [Int]
willDrink = [3, 4]

left :: [Int]
left = pure (-) <*> pack <*> pure drank

needed :: [Int]
needed = pure (*) <*> people <*> willDrink

toBuy :: Int
toBuy = maximum (pure (-) <*> needed <*> left)
