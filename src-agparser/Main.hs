module Main (main) where

import Common
import Conduit ((.|))
import Conduit qualified as C
import Data.Conduit.Serialization.Binary
import Data.HashMap.Strict qualified as M
import Data.Hashable
import Data.List qualified as L
import Dictionary.BetaConv qualified as BetaConv
import Dictionary.Types
import Options.Applicative qualified as Opt
import System.Directory qualified as Dir
import Prelude
import Data.Text (unpack)
import Parser qualified

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
      normalEntries = map toPair entries
      normalMap = group normalEntries
    putStrLn [qq|{length entries} dictionary entries loaded (betacode).|]
    putStrLn [qq|{M.size normalMap} dictionary entries (normalised).|]
    inputText <- readFile inputTextFile
    -- print $ M.keysSet normalMap
    putStrLn inputText
    forM_ (Parser.parse normalMap inputText) $ \variant -> do
      putStrLn "======="
      putStrLn "Variant"
      putStrLn "======="
      forM_ variant $ \xs -> do
        forM_ xs $ \Entry{..} ->
          print entryTerm
        putStrLn "--------"

 where
  -- Open the file
  cliOpts = Opt.info (cliArgs Opt.<**> Opt.helper) Opt.fullDesc
  toPair entry@(Entry{..}) = (unpack $ unNormalisedTerm $ BetaConv.betacodeToNormal entryTerm, entry)

group :: (Eq k, Hashable k) => [(k, v)] -> M.HashMap k [v]
group = groupBase M.empty

groupBase :: (Eq k, Hashable k) => M.HashMap k [v] -> [(k, v)] -> M.HashMap k [v]
groupBase = L.foldl' (\m (k, v) -> inserts k v m)

inserts :: (Eq k, Hashable k) => k -> v -> M.HashMap k [v] -> M.HashMap k [v]
inserts k v = M.insertWith (const (v :)) k [v]
