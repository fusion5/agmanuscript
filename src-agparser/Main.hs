module Main (main) where

import Common
import Conduit ((.|))
import Conduit qualified as C
import Dictionary.Types
import Options.Applicative qualified as Opt
import System.Directory qualified as Dir
import Prelude
import Data.Conduit.Serialization.Binary

newtype CLIArgs = CLIArgs
  { dictionaryFile :: FilePath
  }

cliArgs :: Opt.Parser CLIArgs
cliArgs =
  CLIArgs
    <$> Opt.strOption
      ( Opt.long "dictionary-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Dictionary file in Haskell serialised format"
      )

main :: IO ()
main =
  do
    CLIArgs{..} <- Opt.execParser cliOpts
    putStrLn "I'm the Ancient Greek text parser and translator!"
    exists <- Dir.doesFileExist dictionaryFile
    unless exists $
      error
        [qq|The given dictionary file $dictionaryFile is not accessible|]
    C.runConduitRes $
      C.sourceFile dictionaryFile
        .| conduitDecode @(Entry BetacodeTerm)
        .| C.printC
 where
  -- Open the file
  cliOpts = Opt.info (cliArgs Opt.<**> Opt.helper) Opt.fullDesc
