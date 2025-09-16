module Main (main) where

import Common
import Prelude

import MakeDictionary qualified as Dict
import Options.Applicative qualified as Opt
import System.Directory qualified as Dir

data CLIArgs = CLIArgs
  { dictionaryInputDirectory :: FilePath
  , dictionaryOutputFile :: FilePath
  , fileExtension :: String
  }
  deriving (Show)

cliArgs :: Opt.Parser CLIArgs
cliArgs =
  CLIArgs
    <$> Opt.strOption
      ( Opt.long "input-path"
          <> Opt.metavar "DIR"
          <> Opt.help "Directory of input dictionary files in TEI.2 XML format"
      )
    <*> Opt.strOption
      ( Opt.long "output-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Output file (in binary, serialised format) which can be passed to agparser"
      )
    <*> Opt.strOption
      ( Opt.long "extension"
          <> Opt.metavar "EXTENSION"
          <> Opt.help "Input files from DIR that match the extension (default: .xml)"
          <> Opt.value ".xml"
      )

{- | Outputs to stdout the serialised dictionary built from the TEI.2 XML input
parameters given through the CLI
-}
main :: IO ()
main =
  do
    CLIArgs{..} <- Opt.execParser cliOpts
    exists <- Dir.doesDirectoryExist dictionaryInputDirectory
    unless exists $
      error
        [qq|The given directory path $dictionaryInputDirectory is not accessible|]
    Dict.traverseDictionaryDir dictionaryInputDirectory dictionaryOutputFile
 where
  cliOpts = Opt.info (cliArgs Opt.<**> Opt.helper) Opt.fullDesc
