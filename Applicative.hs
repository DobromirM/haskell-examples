import qualified Data.Map as Map

type LatLong = (Double,Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [
  ("Arkham",(42.6054,-70.7829)),
  ("Innsmouth",(42.8250,-70.8150)),
  ("Carcosa",(29.9714,-90.7694)),
  ("New York",(40.7776,-73.9691))]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double,Double)
latLongToRads (lat,long) = (rlat,rlong)
  where rlat = toRadians lat
        rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
  where (rlat1,rlong1) = latLongToRads coords1
        (rlat2,rlong2) = latLongToRads coords2
        dlat = rlat2 - rlat1
        dlong = rlong2 - rlong1
        a = (sin (dlat/2))^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2
        c = 2 * atan2 (sqrt a) (sqrt (1-a))
        earthRadius = 3961.0

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just x) (Just y) = Just (x + y)
addMaybe _ _ = Nothing

maybeInc = (+) <$> Just 1

main :: IO ()
main = do
  putStrLn "Enter the starting city name:"
  startingInput <- getLine
  let startingCity = Map.lookup startingInput locationDB
  putStrLn "Enter the destination city name:"
  destInput <- getLine
  let destCity = Map.lookup destInput locationDB
  let distance = haversine <$> startingCity <*> destCity
  printDistance distance

---------------------------------------------------------------------------

minOfThree :: Ord a => a -> a -> a -> a
minOfThree f s t = min f (min s t)

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

mainMin :: IO ()
mainMin = do
  putStrLn "Enter three numbers"
  min <- minOfInts
  putStrLn (show min ++ " is the smallest!")

---------------------------------------------------------------------------

data User = User {
  name :: String,
  gamerId :: Int,
  score :: Int
} deriving Show

serverUsername :: Maybe String
serverUsername = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

mainUser :: IO ()
mainUser = do
  putStrLn "Enter a username, gamerId and score"
  user <- User <$> getLine <*> readInt <*> readInt
  print user

-------------------------------------------------------------------------

haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO f s = haversine <$> f <*> s

haversineIOv1 :: IO LatLong -> IO LatLong -> IO Double
haversineIOv1 f s = do
  first <- f
  second <- s
  let result = haversine first second
  return result

------------------------------------------------------------------------

data RobotPart = RobotPart {
  rname :: String,
  description :: String,
  cost :: Double,
  count :: Int
} deriving Show

leftArm :: RobotPart
leftArm = RobotPart {
  rname = "left arm",
  description = "left arm for face punching!",
  cost = 1000.0,
  count = 3
}


rightArm :: RobotPart
rightArm = RobotPart {
  rname = "right arm",
  description = "right arm for kind hand gestures",
  cost = 1025.0,
  count = 5
}

robotHead :: RobotPart
robotHead = RobotPart {
  rname = "robot head",
  description = "this head looks mad",
  cost =  5092.25,
  count = 2
}

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where keys = [1,2,3]
        vals = [leftArm, rightArm, robotHead]
        keyVals = zip keys vals

mainParts :: IO ()
mainParts = do
  putStrLn "Enter parts numbers"
  firstId <- readInt
  secondId <- readInt
  let firstPart = Map.lookup firstId partsDB
  let secondPart = Map.lookup secondId partsDB
  let lowest = min <$> (cost <$> firstPart) <*> (cost <$> secondPart)
  print lowest
