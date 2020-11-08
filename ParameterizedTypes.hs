import qualified Data.Map as Map

data Box a = Box a
  deriving Show

wrap :: a -> Box a
wrap n = Box n

unwrap :: Box a -> a
unwrap (Box n) = n

data Triple a = Triple a a a
  deriving Show

type Point3D = Triple Double
type FullName = Triple String

aPoint :: Point3D
aPoint = Triple 1.0 2.0 3.0

aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

first :: Triple a -> a
first (Triple fst _ _) = fst

second :: Triple a -> a
second (Triple fst snd _) = snd

third :: Triple a -> a
third (Triple _ _ trd) = trd

toList :: Triple a -> [a]
toList (Triple fst snd trd) = [fst, snd, trd]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple fst snd trd) = Triple (f fst) (f snd) (f trd)

data List a = Empty | Cons a (List a)
  deriving Show

normalList1 :: [Int]
normalList1 = 1 : 2 : 3 : []

consList1 :: List Int
consList1 = Cons 1 (Cons 2 (Cons 3 Empty))

consMap :: (a -> b) -> List a -> List b
consMap _ Empty = Empty
consMap f (Cons x xs) = Cons (f x) (consMap f xs)

data Organ = Heart | Brain | Kidney | Spleen
  deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs =  [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

---------------------------------------------------------------------

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box n) = Box (f n)

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple fst snd trd) = Triple (f fst) (f snd) (f trd)

---------------------------------------------------------------------

allOrgans :: [Organ]
allOrgans = [Heart .. Spleen]

organCounts :: [Int]
organCounts = map countOrgans allOrgans
  where countOrgans = \organ -> length (filter (== organ) organs)


organInventory :: Map.Map Organ Int
organInventory = Map.fromList (zip allOrgans organCounts)

