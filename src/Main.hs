module Main where

import Conduit ((.|))
import Control.Monad (void, unless)
import Data.Serialize.Text ()
import Data.Text as T
import Data.XML.Types
import Prelude
import Text.XML.Stream.Parse

import qualified Conduit              as C
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit.List    as CL
import qualified Data.Serialize       as Ser
import qualified Options.Applicative  as Opt
import qualified System.Directory     as Dir
import qualified System.FilePath      as File

import qualified GHC.Generics as GHC

type Conduit i o r = C.ConduitT i o (C.ResourceT IO) r

data CLIArgs = CLIArgs
  { dictionaryInDirectory :: FilePath
  , fileExtension         :: String
  } deriving (Show)

cliArgs :: Opt.Parser CLIArgs
cliArgs
  = CLIArgs
    <$> Opt.strOption
        (   Opt.long "path"
        <>  Opt.metavar "PATH"
        <>  Opt.help "Directory of dictionary files in TEI.2 XML format"
        )
    <*> Opt.strOption
        (   Opt.long "extension"
        <>  Opt.metavar "EXTENSION"
        <>  Opt.help "Keep files from DIR that match the extension (default: .xml)"
        <>  Opt.value ".xml"
        )

newtype Translation = Translation Text
  deriving (Eq, Show, GHC.Generic)

data Entry = Entry Text Translation
  deriving (Eq, Show, GHC.Generic)

instance Ser.Serialize Translation
instance Ser.Serialize Entry

newtype Dictionary
  = Dictionary
    { dEntries :: [Entry]
    }

-- Parses <entryFree>...</entryFree>
parseEntry :: Conduit Event Entry (Maybe ())
parseEntry = tag' "entryFree" attributes entryTag
  where
    attributes
      = (,) <$> requireAttr "key"
            <*> requireAttr "type"
            <*  ignoreAttrs
    entryTag (key, "main")
      = void $ C.mapOutput (Entry key)
          $ many' $ tagIgnoreAttrs "sense"
             $ many' (contentRec >>= (C.yield . Translation) >> pure Nothing)
    entryTag _ = void $ many' (ignoreTree anyName ignoreAttrs)

parseTranslation :: Conduit Event Translation (Maybe ())
parseTranslation = tagIgnoreAttrs "tr" trTag
  where
    trTag
      = do
        x <- contentRec
        C.yield $ Translation x

-- |Returns all content, concatenated, recursively
contentRec :: Conduit Event Translation Text
contentRec
  = T.concat <$> many' (tagIgnoreAttrs anyName contentRec `orE` contentMaybe)

selectDepth :: Conduit Event Entry ()
selectDepth =
  void $ tagIgnoreAttrs "TEI.2"
    $ many' $ tagIgnoreAttrs "text"
      $ many' $ tagIgnoreAttrs "body"
        $ many' $ tagIgnoreAttrs "div0"
          $ many' parseEntry

processDictionaryDir :: FilePath -> IO ()
processDictionaryDir inputDirectory
  = -- runConduitRes and ResourceT don't seem to be needed, except they are required by
    -- sourceDirectory and sourceFile
    C.runConduitRes $
       C.sourceDirectory inputDirectory
    .| C.awaitForever processFile
    .| conduitPut
    .| C.printC

conduitPut :: Ser.Serialize a => Conduit a BSL.ByteString ()
conduitPut = CL.map $ Ser.runPutLazy . Ser.put

processFile :: FilePath -> Conduit a Entry ()
processFile inputPath | File.takeExtension inputPath == ".xml"
 = C.sourceFile inputPath .| parseBytes def .| selectDepth
processFile _ = pure ()

-- |Takes CLI parameters (a directory of xml files) and outputs the serialised dictionary to
-- stdout.
main :: IO ()
main
  = do
    CLIArgs{..} <- Opt.execParser cliOpts
    putStrLn $ "Opening dictionary in directory path: `" <> dictionaryInDirectory <> "'"
    exists <- Dir.doesDirectoryExist dictionaryInDirectory
    unless exists $ error "the given directory path is not accessible"
    processDictionaryDir dictionaryInDirectory
  where
    cliOpts = Opt.info (cliArgs Opt.<**> Opt.helper) Opt.fullDesc
