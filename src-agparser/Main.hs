module Main (main) where

import Common
import Conduit ((.|))
-- TODO: don't import MakeDictionary in a place where we only have to read the dictionary

import Data.ByteString
import Dictionary.Types
import MakeDictionary.Internal
import Prelude

import Conduit qualified as C
import Data.Conduit.List qualified as CL
import Options.Applicative qualified as Opt
import System.Directory qualified as Dir

import Data.Serialize qualified as Ser

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

deserialiseEntry :: Conduit ByteString (Entry BetacodeTerm) ()
deserialiseEntry =
  CL.map
    ( \line -> case Ser.runGet Ser.get line of
        Left err -> error [qq|Deserialisation error for line $line: $err|]
        Right ok -> ok
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
        .| C.lineAsciiC deserialiseEntry
        .| C.printC
 where
  -- Open the file
  cliOpts = Opt.info (cliArgs Opt.<**> Opt.helper) Opt.fullDesc
