{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Semigroup

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

aWord :: T.Text
aWord = "Cheese"

sampleInput :: T.Text
sampleInput = "this\nis\ninput"

myLines :: T.Text -> [T.Text]
myLines text = words
  where words = T.splitOn "\n" text

myUnlines :: [T.Text] -> T.Text
myUnlines words = text
  where text = T.intercalate "\n" words

dharma :: T.Text
dharma = "धर्म"

bgText :: T.Text
bgText = "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।स्वधर्मे निधनं श्रेयः परधर्म"

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where pieces = T.splitOn query fullText
        highlighted = mconcat ["{", query, "}"]

---------------------------------------------------------------

helloPerson :: T.Text -> T.Text
helloPerson name = "Hello" <> " " <> name <> "!"

main :: IO ()
main = do
  TIO.putStrLn "Hello! What's your name?"
  name <- TIO.getLine
  let statement = helloPerson name
  TIO.putStrLn statement


--------------------------------------------------------------

toInts :: T.Text -> [Int]
toInts = map (read . T.unpack) . T.lines

mainLazy :: IO ()
mainLazy = do
  userInput <- TIO.getContents
  let numbers = toInts userInput
  TIO.putStrLn (( T.pack . show . sum) numbers)
