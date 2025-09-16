module MakeDictionary (
  traverseDictionaryDir,
  MakeDictionary.Internal.parseEntry,
) where

import Conduit ((.|))
import Conduit qualified as C
-- import Data.Binary.Builder
import Data.Conduit.Combinators qualified as C
-- import Data.Conduit.List qualified as CL
-- import Data.Serialize qualified as Ser
import Data.Serialize.Text ()
import MakeDictionary.Internal
import Prelude
import Data.Conduit.Serialization.Binary

-- serialise :: (Ser.Serialize a) => Conduit a Builder ()
-- serialise = CL.map $ Ser.execPut . Ser.put

-- | Outputs to stdout the dictionary in serialised format
traverseDictionaryDir :: FilePath -> IO ()
traverseDictionaryDir inputDirectory =
  -- runConduitRes and ResourceT don't seem to be needed, except they are required by
  -- sourceDirectory and sourceFile
  C.runConduitRes $
    C.sourceDirectory inputDirectory
      .| C.awaitForever processFile
      -- .| serialise
      -- .| C.builderToByteString
      .| conduitEncode
      .| C.stdout
