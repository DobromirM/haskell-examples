data StoreItem = BookItem Book | VinylItem VinylRecord | ToyItem CollectibleToy | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (VinylItem vinyl) = recordPrice vinyl
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0.0

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (VinylItem vinyl) = show (artist vinyl)
madeBy (PamphletItem pamphlet) = show (contactDetails pamphlet)
madeBy _ = "Unknown creator"

data Book = Book {
    author :: Creator
  , isbn   :: String
  , bookTitle  :: String
  , bookYear   :: Int
  , bookPrice  :: Double }

data VinylRecord = VinylRecord {
    artist  ::  Creator
  , recordTitle :: String
  , recordYear :: Int
  , recordPrice :: Double }


data CollectibleToy = CollectibleToy {
    name :: String
  , description :: String
  , toyPrice :: Double }

data Pamphlet = Pamphlet {
    title :: String
  , pamphletDesc :: String
  , contactDetails :: String }

data Creator = AuthorCreator Author | ArtistCreator Artist
  deriving Show

data Author = Author Name
  deriving Show

data Artist = Artist Name | Band String
  deriving Show

data Name = Name FirstName LastName
        | NameWithMiddle FirstName MiddleName LastName
        | TwoInitialsWithLast Initial Initial LastName
        deriving Show

type FirstName = String
type LastName = String
type MiddleName = String
type Initial = Char

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))