module MakeDictionary
  ( traverseDictionaryDir
  ) where

import Conduit ((.|))
import Data.Serialize.Text ()
import Prelude
import MakeDictionary.Internal

import qualified Conduit              as C
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit.List    as CL
import qualified Data.Serialize       as Ser

serialise :: Ser.Serialize a => Conduit a BSL.ByteString ()
serialise = CL.map $ Ser.runPutLazy . Ser.put

-- Outputs to stdout the dictionary in serialised format
traverseDictionaryDir :: FilePath -> IO ()
traverseDictionaryDir inputDirectory
  = -- runConduitRes and ResourceT don't seem to be needed, except they are required by
    -- sourceDirectory and sourceFile
    C.runConduitRes $
       C.sourceDirectory inputDirectory
    .| C.awaitForever processFile
    .| serialise
    .| C.printC
