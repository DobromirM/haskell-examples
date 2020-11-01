import Data.List
import Data.Semigroup

myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll f = (foldr (&&) True) . (map f)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = (foldr (||) False) . (map f)


data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown | Clear
  deriving (Show, Eq)

instance Semigroup Color where
  (<>)  any Clear = any
  (<>)  Clear any = any
  (<>)  Red Blue = Purple
  (<>)  Blue Red = Purple
  (<>)  Yellow Blue = Green
  (<>)  Blue Yellow = Green
  (<>)  Yellow Red = Orange
  (<>)  Red Yellow = Orange
  (<>)  a b | a == b = a
            | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
            | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
            | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
            | otherwise = Brown

instance Monoid Color where
  mempty = Clear
  mappend = (<>)

---------------------------------------------------------------------

data Events = Events [String]

instance Semigroup Events where
  (<>) (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
    where combiner = (\x y -> mconcat [x, "-", y])

instance Monoid Events where
  mempty = Events []
  mappend = (<>)

data Probs = Probs [Double]

instance Semigroup Probs where
  (<>) (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

instance Monoid Probs where
  mempty = Probs []
  mappend = (<>)

data PTable = PTable Events Probs

instance Show PTable where
  show (PTable (Events events) (Probs probs)) = mconcat (zipWith showPair events probs)

instance Semigroup PTable where
  (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1
  (<>) (PTable (Events []) (Probs [])) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = PTable (e1 <> e2) (p1 <> p2)

instance Monoid PTable where
  mempty = PTable (Events []) (Probs [])
  mappend = (<>)

createPTable :: Events -> Probs -> PTable
createPTable events (Probs probs) = PTable events (Probs normalisedProbs)
  where totalProbs = sum probs
        normalisedProbs = map (\x -> x / totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where nToAdd = length l2
        repeatedL1 = map (take nToAdd . repeat) l1
        newL1 = mconcat repeatedL1
        cycledL2 = cycle l2

---------------------------------------------------------------------------

coin :: PTable
coin = createPTable (Events ["heads","tails"]) (Probs [0.5,0.5])

spinner :: PTable
spinner = createPTable (Events ["red","blue","green"]) (Probs [0.1,0.2,0.7])