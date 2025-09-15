module Main (main) where

import Common
import Prelude

import qualified Options.Applicative as Opt
import qualified System.Directory as Dir

newtype CLIArgs = CLIArgs
  { dictionaryFile :: FilePath
  }

cliArgs :: Opt.Parser CLIArgs
cliArgs =
  CLIArgs
    <$> Opt.strOption
      ( Opt.long "dictionary-file"
        <> Opt.metavar "FILE"
        <> Opt.help "Dictionary file in Haskell serialised format")

main :: IO ()
main =
  do
    CLIArgs{..} <- Opt.execParser cliOpts
    putStrLn "I'm the Ancient Greek text parser and translator!"
    exists <- Dir.doesFileExist dictionaryFile
    unless exists $
      error
        [qq|The given dictionary file $dictionaryFile is not accessible|]

 where
  cliOpts = Opt.info (cliArgs Opt.<**> Opt.helper) Opt.fullDesc
