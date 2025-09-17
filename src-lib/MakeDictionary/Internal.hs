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
import Data.Text as T
import Dictionary

import Conduit qualified as C
import Data.XML.Types qualified as XMLStream
import System.FilePath qualified as File
import Text.XML.Stream.Parse qualified as XML

type Conduit i o r = C.ConduitT i o (C.ResourceT IO) r

-- | Parses <entryFree>...</entryFree>
parseEntry :: Conduit XMLStream.Event (Entry BetacodeTerm) (Maybe ())
parseEntry = XML.tag' "entryFree" attributes entryTag
 where
  attributes =
    (,) <$> XML.requireAttr "key" <*> XML.requireAttr "type" <* XML.ignoreAttrs
  entryTag (key, "main") =
    void $
      C.mapOutput (Entry (BetacodeTerm key)) $
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

selectDepth :: Conduit XMLStream.Event (Entry BetacodeTerm) ()
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

processFile :: FilePath -> Conduit a (Entry BetacodeTerm) ()
processFile inputPath
  | File.takeExtension inputPath == ".xml" =
      C.sourceFile inputPath .| XML.parseBytes XML.def .| selectDepth
processFile _ = pure ()
