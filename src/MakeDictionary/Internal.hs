module MakeDictionary.Internal (
  Conduit,
  Entry (..),
  Translation (..),
  parseEntry,
  processFile,
  selectDepth,
)
where

import Common
import Conduit ((.|))
import Data.Serialize.Text ()
import Data.Text as T
import Dictionary.Types

import qualified Conduit as C
import qualified Data.XML.Types as XMLStream
import qualified System.FilePath as File
import qualified Text.XML.Stream.Parse as XML

type Conduit i o r = C.ConduitT i o (C.ResourceT IO) r

-- Parses <entryFree>...</entryFree>
parseEntry :: Conduit XMLStream.Event Entry (Maybe ())
parseEntry = XML.tag' "entryFree" attributes entryTag
 where
  attributes =
    (,) <$> XML.requireAttr "key" <*> XML.requireAttr "type" <* XML.ignoreAttrs
  entryTag (key, "main") =
    void $
      C.mapOutput (Entry key) $
        XML.many' $
          XML.tagIgnoreAttrs "sense" $
            XML.many' (contentRec >>= (C.yield . Translation) >> pure Nothing)
  entryTag _ = void $ XML.many' (XML.ignoreTree XML.anyName XML.ignoreAttrs)

-- | Returns all content, concatenated, recursively
contentRec :: Conduit XMLStream.Event Translation Text
contentRec =
  T.concat
    <$> XML.many'
      (XML.tagIgnoreAttrs XML.anyName contentRec `XML.orE` XML.contentMaybe)

selectDepth :: Conduit XMLStream.Event Entry ()
selectDepth =
  void $
    XML.tagIgnoreAttrs "TEI.2" $
      XML.many' $
        XML.tagIgnoreAttrs "text" $
          XML.many' $
            XML.tagIgnoreAttrs "body" $
              XML.many' $
                XML.tagIgnoreAttrs "div0" $
                  XML.many' parseEntry

processFile :: FilePath -> Conduit a Entry ()
processFile inputPath
  | File.takeExtension inputPath == ".xml" =
      C.sourceFile inputPath .| XML.parseBytes XML.def .| selectDepth
processFile _ = pure ()
