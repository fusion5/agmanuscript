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

import Conduit qualified as C
import Data.Map qualified as Map
import Dictionary.BetaConv qualified as BetaConv
import Dictionary.Types qualified as D
import MakeDictionary qualified as MD
import Parser qualified

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
    dictionaryParseSpec
    parseTextSpec
    betaconvSpec

dictionaryParseSpec :: Spec
dictionaryParseSpec = describe "TEI format dictionary parsing" $ do
  it "Parse entryFree0.xml" $ do
    testOnFile "./test-data/entryFree0.xml" (void MD.parseEntry)
      `shouldReturn` [ D.Entry
                        (D.BetacodeTerm "key1")
                        (D.Translation "translation1")
                     ]
  it "Parse entryFree1.xml" $ do
    testOnFile "./test-data/entryFree1.xml" (void $ many' MD.parseEntry)
      `shouldReturn` [ D.Entry
                        (D.BetacodeTerm "key1")
                        (D.Translation "translation1")
                     ]

data Token = A | B | AA | AAA
  deriving (Show, Eq)

parseTextSpec :: Spec
parseTextSpec = describe "Text parsing" $ do
  describe "Given a map of unambiguous terms and an input text containing the terms" $ do
    let
      terms = Map.insert "aa" A $ Map.insert "bbb" B Map.empty
    it "Then the empty string is recognised but doesn't produce tokens" $ do
      Parser.parse terms "" `shouldBe` [[]]
    it "Then something not in the map is not recognised" $ do
      Parser.parse terms "c" `shouldBe` []
    it "Then a single term is recognised" $ do
      Parser.parse terms "aa" `shouldBe` [[A]]
      Parser.parse terms "bbb" `shouldBe` [[B]]
    it "And a sequence of terms is recognised" $ do
      Parser.parse terms "aabbb" `shouldBe` [[A, B]]
      Parser.parse terms "bbbaa" `shouldBe` [[B, A]]
  describe "Given a map of ambiguous terms and an input text containing the terms" $ do
    let
      terms =
        Map.insert "b" B $
          Map.insert "a" A $
            Map.insert "aa" AA Map.empty
    it "Then the parser outputs all possible interpretations" $ do
      Parser.parse terms "aa" `shouldBe` [[A, A], [AA]]
      Parser.parse terms "baa" `shouldBe` [[B, A, A], [B, AA]]
      Parser.parse terms "aab" `shouldBe` [[A, A, B], [AA, B]]

betaconvSpec :: Spec
betaconvSpec = do
  describe "Betacode conversion" $ do
    it "Any betacode word should be correctly translated to the Roman alphabet" $ do
      BetaConv.betacodeToNormalEntry
        (D.Entry (D.BetacodeTerm "e(/kaston") (D.Translation ""))
        `shouldBe` D.Entry (D.NormalisedTerm "ekaston") (D.Translation "")
