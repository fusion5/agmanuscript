module MakeDictionary (
  traverseDictionaryDir,
  MakeDictionary.Internal.parseEntry,
) where

import Conduit ((.|))
import Conduit qualified as C
import Data.Conduit.Serialization.Binary
import Data.Serialize.Text ()
import MakeDictionary.Internal
import Prelude

-- | Outputs to stdout the dictionary in serialised format
traverseDictionaryDir :: FilePath -> FilePath -> IO ()
traverseDictionaryDir inputDirectory outputFile =
  -- runConduitRes and ResourceT don't seem to be needed, except they are required by
  -- sourceDirectory and sourceFile
  C.runConduitRes $
    C.sourceDirectory inputDirectory
      .| C.awaitForever processFile
      .| conduitEncode
      .| C.sinkFile outputFile
