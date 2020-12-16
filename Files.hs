{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


main :: IO ()
main = do
  helloFile <- openFile "hello.txt" ReadMode
  firstLine <- hGetLine helloFile
  putStrLn firstLine
  secondLine <- hGetLine helloFile
  goodbyeFile <- openFile "goodbye.txt" WriteMode
  hPutStrLn goodbyeFile secondLine
  hClose helloFile
  hClose goodbyeFile
  putStrLn "done!"

mainEmpty :: IO ()
mainEmpty = do
  helloFile <- openFile "hello.txt" ReadMode
  hasLine <- hIsEOF helloFile
  firstLine <- if not hasLine
               then hGetLine helloFile
               else return "emtpy"

  hasLine <- hIsEOF helloFile
  secondLine <- if not hasLine
                then hGetLine helloFile
                else return "empty"

  putStrLn firstLine
  putStrLn secondLine
  putStrLn "done!"

-----------------------------------------------------------------

getCounts :: String -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
  where charCount = length input
        wordCount = (length . words) input
        lineCount = (length . lines) input

countsText :: (Int, Int, Int) -> String
countsText (cc, wc, lc) = unwords ["chars: "
                                   , show cc
                                   , "words: "
                                   , show wc
                                   , "lines: "
                                   , show lc]

mainFile :: IO ()
mainFile = do
  args <- getArgs
  let fileName = head args
  input <- readFile fileName
  let summary = (countsText . getCounts) input
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
  putStrLn summary


----------------------------------------------------------------------------

mainFileErr :: IO ()
mainFileErr = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  input <- hGetContents file
  let summary = (countsText . getCounts) input
  putStrLn summary
  hClose file
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])

-----------------------------------------------------------------------------

getCountsT :: T.Text -> (Int, Int, Int)
getCountsT input = (charCount, wordCount, lineCount)
  where charCount = T.length input
        wordCount = (length . T.words) input
        lineCount = (length . T.lines) input

countsTextT :: (Int, Int, Int) -> T.Text
countsTextT (cc, wc, lc) = T.pack (unwords ["chars: "
                                       , show cc
                                       , "words: "
                                       , show wc
                                       , "lines: "
                                       , show lc])

mainFileT :: IO ()
mainFileT = do
  args <- getArgs
  let fileName = head args
  input <- TIO.readFile fileName
  let summary = (countsTextT . getCountsT) input
  TIO.appendFile "stats.dat" (mconcat [(T.pack fileName), " ", summary, "\n"])
  TIO.putStrLn summary

-------------------------------------------------------------------------------

mainCp :: IO ()
mainCp = do
  args <- getArgs
  let source = args !! 0
  let dest = args !! 1
  input <- TIO.readFile source
  TIO.appendFile dest input

-------------------------------------------------------------------------------

mainCapitalise :: IO ()
mainCapitalise = do
  args <- getArgs
  let fileName = head args
  input <- TIO.readFile fileName
  TIO.writeFile fileName (T.toUpper input)