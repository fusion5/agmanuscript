module Main where

import Text.XML.HaXml

import qualified Options.Applicative as Opt

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

main :: IO ()
main
  = do
    print "Input file:"
    print =<< Opt.execParser cliOpts
    print "XML Parse result:"
    print $ xmlParse fn body
  where
    cliOpts = Opt.info (cliArgs Opt.<**> Opt.helper) Opt.fullDesc
    fn   = "<TEST>"
    body =  "<ul><li>1</li><li>2</li></ul>"
