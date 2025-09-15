module Main (main) where

data CLIArgs = CLIArgs
  { dictionaryFile :: FilePath
  }

main :: IO ()
main =
  do
    putStrLn "I'm the Ancient Greek text parser!"
