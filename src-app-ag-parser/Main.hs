module Main (main) where

import Common
import Conduit ((.|))
import Conduit qualified as C
import Data.Conduit.Serialization.Binary
import Data.HashMap.Strict qualified as M
import Data.Text (unpack)
import Dictionary
import Options.Applicative qualified as Opt
import Parser qualified
import System.Directory qualified as Dir
import Prelude

data CLIArgs = CLIArgs
  { inputDictionaryFile :: FilePath
  , inputTextFile :: FilePath
  }

cliArgs :: Opt.Parser CLIArgs
cliArgs =
  CLIArgs
    <$> Opt.strOption
      ( Opt.long "input-dictionary-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Dictionary file in Haskell serialised format"
      )
    <*> Opt.strOption
      ( Opt.long "input-text-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Input text file containing the words we are trying to recognise without spaces."
      )

main :: IO ()
main =
  do
    CLIArgs{..} <- Opt.execParser cliOpts
    putStrLn "I'm the Ancient Greek text parser and translator!"
    existsDictionary <- Dir.doesFileExist inputDictionaryFile
    existsText <- Dir.doesFileExist inputTextFile
    unless existsDictionary $
      error [qq|The given dictionary file $inputDictionaryFile is not accessible|]
    unless existsText $
      error [qq|The given dictionary file $inputTextFile is not accessible|]
    entries <-
      C.runConduitRes $
        C.sourceFile inputDictionaryFile
          .| conduitDecode @(Entry BetacodeTerm)
          .| C.sinkList
    let
      normalMap = entriesToDictionary entries
      parserMap = M.mapKeys (unpack . unNormalisedTerm) normalMap
    putStrLn [qq|{length entries} dictionary entries loaded (betacode).|]
    putStrLn [qq|{M.size normalMap} dictionary entries (normalised).|]
    inputText <- readFile inputTextFile
    putStrLn inputText
    forM_ (Parser.parse parserMap inputText) $ \variant -> do
      putStrLn "======="
      putStrLn "Variant"
      putStrLn "======="
      forM_ variant $ \xs -> do
        forM_ xs $ \(Translation body) ->
          print body
        putStrLn "--------"
 where
  cliOpts = Opt.info (cliArgs Opt.<**> Opt.helper) Opt.fullDesc
