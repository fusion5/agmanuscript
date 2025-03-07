module Main where

import Conduit ((.|))
import Control.Monad (void)
import Prelude
import Text.XML.Stream.Parse
import Data.XML.Types
import Data.Text as T

import qualified Conduit as C  -- (runConduit, (.|))
import qualified Options.Applicative as Opt
import qualified System.Directory    as Dir

import Data.ByteString.Lazy

-- type FilePath = String

data CLIArgs = CLIArgs
  { dictionaryInFile :: FilePath
  } deriving (Show)

cliArgs :: Opt.Parser CLIArgs
cliArgs
  = CLIArgs
    <$> Opt.strOption
          (   Opt.long "dictionary-in"
          <>  Opt.metavar "FILE"
          <>  Opt.help "Input dictionary file in XML format"
          )

data Translation = Translation Text
  deriving Show

data Entry = Entry Text Translation
  deriving Show

testInput :: ByteString
testInput = "<TEI.2><text><header>bllaal</header><body><div0> <bla />  \
  \ <entryFree key=\"key1\" type=\"main\" h=\"q\">entry1<orth x=\"y\"><bbaa>uxy</bbaa></orth><sense><tr>translation1</tr></sense></entryFree>  \
  \ <entryFree key=\"key2\" type=\"main\">        entry2<orth>zzz</orth>                     <sense>translation2</sense></entryFree>  \
  \ </div0></body></text></TEI.2>"

-- >>> test
test :: IO ()
test = C.runConduit $ parseLBS def testInput .| selectDepth .| C.printC

testOn :: Show a => ByteString -> C.ConduitT Event a IO () -> IO ()
testOn bs conduit = C.runConduit $ parseLBS def bs .| conduit .| C.printC

-- Parses <entryFree>...</entryFree>
parseEntry :: C.ConduitT Event Entry IO (Maybe ())
parseEntry = tag' "entryFree" attributes entryTag
  where
    attributes
      = (,) <$> requireAttr "key"
            <*> requireAttr "type"
            <*  ignoreAttrs
    entryTag (key, "main")
      = do
        void $ C.mapOutput (Entry key) $ many' $ tagIgnoreAttrs "sense"
             $ many' parseTranslation
    entryTag _ = pure ()

-- Parses <tr>...</tr> -- returns the contents of the tr
-- >>> testOn "<tr>haaa</tr>" (parseTranslation >> pure ())
-- >>> testOn "<tr>a<u>b</u>c</tr>" (parseTranslation >> pure ())
parseTranslation :: C.ConduitT Event Translation IO (Maybe ())
parseTranslation = tagIgnoreAttrs "tr" trTag
  where
    trTag
      = do
        x <- contentRec
        C.yield $ Translation x

contentRec :: C.ConduitT Event Translation IO Text
contentRec
  = T.concat <$> many' (tagIgnoreAttrs anyName contentRec `orE` contentMaybe)

selectDepth :: C.ConduitT Event Entry IO ()
selectDepth =
  void $ tagIgnoreAttrs "TEI.2"
    $ many' $ tagIgnoreAttrs "text"
      $ many' $ tagIgnoreAttrs "body"
        $ many' $ tagIgnoreAttrs "div0"
          $ many' parseEntry

processDictionary :: FilePath -> IO ()
processDictionary inputFile = do
  void $ C.withSourceFile inputFile $ \fileSource ->
    C.runConduit
       $ fileSource
      .| parseBytes def
      .| selectDepth
      .| C.printC

main :: IO ()
main
  = do
    CLIArgs{..} <- Opt.execParser cliOpts
    putStrLn $ "Opening input file: `" <> dictionaryInFile <> "'"
    exists <- Dir.doesFileExist dictionaryInFile
    if exists
      then do
        putStrLn "The file exists..."
        perm <- Dir.getPermissions  dictionaryInFile
        print perm
        processDictionary dictionaryInFile
      else
        putStrLn "File does not exist"
  where
    cliOpts = Opt.info (cliArgs Opt.<**> Opt.helper) Opt.fullDesc
    -- fn   = "<TEST>"
    -- body =  "<ul><li>1</li><li>2</li></ul>"
