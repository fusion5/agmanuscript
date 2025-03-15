{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module MainTest where

import Conduit ((.|))
import Data.ByteString.Lazy
import Data.XML.Types
import Prelude
import Test.Hspec
import Test.QuickCheck.Instances.Natural ()
import Text.XML.Stream.Parse
import Control.Monad (void)

import qualified Main
import qualified Conduit as C

type Conduit i o r = C.ConduitT i o (C.ResourceT IO) r

testOn :: Show a => ByteString -> Conduit Event a () -> IO [a]
testOn bs conduit
  = C.runConduitRes $ parseLBS def bs .| conduit .| C.sinkList

testOnFile :: Show a => FilePath -> Conduit Event a () -> IO [a]
testOnFile inputFile conduit
  = C.withSourceFile inputFile $ \fileSource ->
      C.runConduitRes $ fileSource .| parseBytes def .| conduit .| C.sinkList

main :: IO ()
main = hspec $
  do
    parseEntriesSpec

parseEntriesSpec :: Spec
parseEntriesSpec
  = do
      it "Parse entryFree0.xml" $ do
        testOnFile "./test-data/entryFree0.xml" (void Main.parseEntry)
          `shouldReturn`
            [Main.Entry "key1" (Main.Translation "translation1")]
      it "Parse entryFree1.xml" $ do
        testOnFile "./test-data/entryFree1.xml" (void $ many' Main.parseEntry)
          `shouldReturn`
            [Main.Entry "key1" (Main.Translation "translation1")]
