class Describable a where
  describe :: a -> String

data Icecream = Chocolate | Vanilla
  deriving (Show, Eq, Ord)

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
              then minBound
              else succ n

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6
  deriving (Eq, Ord, Enum)

instance Show SixSidedDie where
  show S1 = "I"
  show S2 = "II"
  show S3 = "III"
  show S4 = "IV"
  show S5 = "V"
  show S6 = "VI"

data TwoSidedDie = One | Two

instance Show TwoSidedDie where
  show One = "I"
  show Two = "II"

data Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where
  compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

names :: [Name]
names = [Name ("Emil","Cioran")
         , Name ("Eugene","Thacker")
         , Name ("Friedrich","Nietzsche")]

data Days = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Enum, Show)

instance Eq Days where
  (==) f s = fromEnum f == fromEnum s

instance Ord Days where
  compare f s = compare (fromEnum f) (fromEnum s)

data FiveSidedDie = Side1 | Side2 | Side3 | Side4 | Side5
  deriving (Show, Enum, Eq, Ord)

class Die a where
  combine :: a -> a -> Int

instance Die FiveSidedDie where
  combine f s = fromEnum f + fromEnum s