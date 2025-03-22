module Main (main) where

import Common
import Prelude

import qualified MakeDictionary as Dict
import qualified Options.Applicative as Opt
import qualified System.Directory as Dir

data CLIArgs = CLIArgs
  { dictionaryInDirectory :: FilePath
  , fileExtension :: String
  }
  deriving (Show)

cliArgs :: Opt.Parser CLIArgs
cliArgs =
  CLIArgs
    <$> Opt.strOption
      ( Opt.long "path"
          <> Opt.metavar "PATH"
          <> Opt.help "Directory of dictionary files in TEI.2 XML format"
      )
    <*> Opt.strOption
      ( Opt.long "extension"
          <> Opt.metavar "EXTENSION"
          <> Opt.help "Keep files from DIR that match the extension (default: .xml)"
          <> Opt.value ".xml"
      )

main :: IO ()
main =
  do
    CLIArgs{..} <- Opt.execParser cliOpts
    exists <- Dir.doesDirectoryExist dictionaryInDirectory
    unless exists $
      error
        [qq|The given directory path $dictionaryInDirectory is not accessible|]
    Dict.traverseDictionaryDir dictionaryInDirectory
 where
  cliOpts = Opt.info (cliArgs Opt.<**> Opt.helper) Opt.fullDesc
