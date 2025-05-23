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
import qualified Data.Map as Map
import qualified Dictionary.Types as Dictionary
import qualified MakeDictionary as MD
import qualified Parser

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
    parseTextSpec

makeDictionarySpec :: Spec
makeDictionarySpec =
  do
    it "Parse entryFree0.xml" $ do
      testOnFile "./test-data/entryFree0.xml" (void MD.parseEntry)
        `shouldReturn` [Dictionary.Entry "key1" (Dictionary.Translation "translation1")]
    it "Parse entryFree1.xml" $ do
      testOnFile "./test-data/entryFree1.xml" (void $ many' MD.parseEntry)
        `shouldReturn` [Dictionary.Entry "key1" (Dictionary.Translation "translation1")]

parseTextSpec :: Spec
parseTextSpec =
  do
    describe "Given a map of unambiguous terms and an input text containing the terms" $ do
      let
        terms = Map.insert "aa" Parser.A $ Map.insert "bbb" Parser.B Map.empty
      it "Then the empty string is not recognised" $ do
        Parser.parse terms "" `shouldBe` [[]]
      it "Then something not in the map is not recognised" $ do
        Parser.parse terms "c" `shouldBe` []
      it "Then a single term is recognised" $ do
        Parser.parse terms "aa" `shouldBe` [[Parser.A]]
        Parser.parse terms "bbb" `shouldBe` [[Parser.B]]
      it "And a sequence of terms is recognised" $ do
        Parser.parse terms "aabbb" `shouldBe` [[Parser.B, Parser.A]]
        Parser.parse terms "bbbaa" `shouldBe` [[Parser.A, Parser.B]]
    describe "Given a map of ambiguous terms and an input text containing the terms" $ do
      let
        terms =
          Map.insert "a" Parser.A $
            Map.insert "aa" Parser.AA Map.empty
      it "Then the parser outputs all possible interpretations" $ do
        Parser.parse terms "aa" `shouldBe` [[Parser.A, Parser.A], [Parser.AA]]
