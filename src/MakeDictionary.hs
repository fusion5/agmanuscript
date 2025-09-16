module MakeDictionary (
  traverseDictionaryDir,
  MakeDictionary.Internal.parseEntry,
) where

import Conduit ((.|))
import Conduit qualified as C
import Data.Conduit.Combinators qualified as C
import Data.Serialize.Text ()
import MakeDictionary.Internal
import Prelude
import Data.Conduit.Serialization.Binary

-- | Outputs to stdout the dictionary in serialised format
traverseDictionaryDir :: FilePath -> IO ()
traverseDictionaryDir inputDirectory =
  -- runConduitRes and ResourceT don't seem to be needed, except they are required by
  -- sourceDirectory and sourceFile
  C.runConduitRes $
    C.sourceDirectory inputDirectory
      .| C.awaitForever processFile
      .| conduitEncode
      .| C.stdout
