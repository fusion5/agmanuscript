{-# LANGUAGE FlexibleInstances #-}

module MainTest where

import Conduit ((.|))
import Control.Monad (void)
import Data.ByteString.Lazy
import Data.XML.Types
import Test.Hspec
import Test.QuickCheck.Instances.Natural ()
import Text.XML.Stream.Parse
import Prelude

import qualified Conduit as C
import qualified Dictionary.Types as Dictionary
import qualified MakeDictionary as MD

type Conduit i o r = C.ConduitT i o (C.ResourceT IO) r

testOn :: (Show a) => ByteString -> Conduit Event a () -> IO [a]
testOn bs conduit =
  C.runConduitRes $ parseLBS def bs .| conduit .| C.sinkList

testOnFile :: (Show a) => FilePath -> Conduit Event a () -> IO [a]
testOnFile inputFile conduit =
  C.withSourceFile inputFile $ \fileSource ->
    C.runConduitRes $ fileSource .| parseBytes def .| conduit .| C.sinkList

main :: IO ()
main = hspec $
  do
    makeDictionarySpec

makeDictionarySpec :: Spec
makeDictionarySpec =
  do
    it "Parse entryFree0.xml" $ do
      testOnFile "./test-data/entryFree0.xml" (void MD.parseEntry)
        `shouldReturn` [Dictionary.Entry "key1" (Dictionary.Translation "translation1")]
    it "Parse entryFree1.xml" $ do
      testOnFile "./test-data/entryFree1.xml" (void $ many' MD.parseEntry)
        `shouldReturn` [Dictionary.Entry "key1" (Dictionary.Translation "translation1")]
