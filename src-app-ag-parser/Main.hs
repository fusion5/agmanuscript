module Main (main) where

import Common
import Conduit ((.|))
import Conduit qualified as C
import Data.Conduit.Serialization.Binary
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (isSubsequenceOf)
import Data.Set qualified as OS
import Data.Text (unpack)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Dictionary
import Options.Applicative qualified as Opt
import Parser qualified
import System.Directory qualified as Dir
import Prelude

data CLIArgs = CLIArgs
  { inputDictionaryFile :: FilePath
  , inputTextFileMaybe :: Maybe FilePath
  , interactive :: Bool
  }

cliArgs :: Opt.Parser CLIArgs
cliArgs =
  CLIArgs
    <$> Opt.strOption
      ( Opt.long "input-dictionary-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Dictionary file in Haskell serialised format"
      )
    <*> Opt.optional
      ( Opt.strOption
          ( Opt.long "input-text-file"
              <> Opt.metavar "FILE"
              <> Opt.help "Input text file containing the words we are trying to recognise without spaces."
          )
      )
    <*> Opt.switch
      ( Opt.long "interactive"
          <> Opt.help "Lookup words interactively instead of parsing the dictionary"
      )

repl :: M.HashMap NormalisedTerm (S.HashSet Translation) -> OS.Set NormalisedTerm -> IO ()
repl dictionary keys = do
  putStrLn
    "Enter :l <word> (lookup exact), :p <word> (prefix word), :s <word> (subsequence word), :e <string> (parsing), :q (quit)"
  command <- T.getLine
  case T.unpack command of
    (':' : 'l' : ' ' : word) -> do
      case M.lookup (NormalisedTerm $ T.pack word) dictionary of
        Nothing -> do
          putStrLn "Exact word not found"
        Just meanings ->
          forM_ meanings $ \(Translation{..}) -> do
            T.putStrLn unTranslation
            T.putStrLn "---"
      repl dictionary keys
    (':' : 'p' : ' ' : word) -> do
      forM_ keys $ \(NormalisedTerm key) -> when (T.pack word `T.isPrefixOf` key) $ T.putStrLn key
      repl dictionary keys
    (':' : 's' : ' ' : word) -> do
      forM_ keys $ \(NormalisedTerm key) ->
        when (word `isSubsequenceOf` T.unpack key) $ T.putStrLn key
      repl dictionary keys
    (':' : 'e' : ' ' : word) ->
      let
        parserMap = M.mapKeys (unpack . unNormalisedTerm) dictionary
       in
        do
          forM_ (Parser.parse parserMap word) $ \variant -> do
            putStrLn "Variant"
            forM_ variant $ \meaning -> print meaning
          repl dictionary keys
    (':' : 'q' : ' ' : _) -> pure ()
    ":q" -> pure ()
    _ -> do
      putStrLn "Unknown command"
      repl dictionary keys

main :: IO ()
main =
  do
    CLIArgs{..} <- Opt.execParser cliOpts
    putStrLn "I'm the Ancient Greek text parser and translator!"
    existsDictionary <- Dir.doesFileExist inputDictionaryFile
    unless existsDictionary $
      error [qq|The given dictionary file $inputDictionaryFile is not accessible|]
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
    if interactive
      then do
        putStrLn "Interactive mode selected"
        repl normalMap (OS.fromList $ M.keys normalMap)
      else do
        case inputTextFileMaybe of
          Nothing -> putStrLn "Input text file not given"
          Just inputTextFile -> do
            existsText <- Dir.doesFileExist inputTextFile
            unless existsText $
              error [qq|The given input text file $inputTextFile is not accessible|]
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
