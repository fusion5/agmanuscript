module Main where


import Prelude

import qualified Text.XML.HaXml      as XML
import qualified Options.Applicative as Opt
import qualified System.IO           as IO

-- type FilePath = String

data CLIArgs = CLIArgs
  { dictionaryIn :: FilePath
  } deriving (Show)

cliArgs :: Opt.Parser CLIArgs
cliArgs
  = CLIArgs
    <$> Opt.strOption
          (   Opt.long "dictionary-in"
          <>  Opt.metavar "FILE"
          <>  Opt.help "Input dictionary file in XML format"
          )

processDictionary :: FilePath -> IO.Handle -> IO ()
processDictionary fileName hFile = do
  body <- IO.hGetContents hFile
  putStrLn "XML Parse result:"
  print $ XML.xmlParse fileName body

main :: IO ()
main
  = do
    CLIArgs{..} <- Opt.execParser cliOpts
    putStrLn $ "Opening input file: " <> dictionaryIn
    IO.withFile dictionaryIn IO.ReadMode $
      processDictionary dictionaryIn
  where
    cliOpts = Opt.info (cliArgs Opt.<**> Opt.helper) Opt.fullDesc
    -- fn   = "<TEST>"
    -- body =  "<ul><li>1</li><li>2</li></ul>"
