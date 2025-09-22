module Dictionary (
  BetacodeTerm (..),
  Entry (..),
  NormalisedTerm (..),
  Translation (..),
  entriesToDictionary,
  betacodeToNormal,
  betacodeToNormalEntry,
) where

import Common
import Data.Binary
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.Hashable
import Data.String
import Data.Text

newtype Translation = Translation {
    unTranslation :: Text
  }
  deriving (Eq, Show, Generic, Hashable)

{- | Term containing betacode content. Example betacode word:
`*(iakunqotro/fos`
-}
newtype BetacodeTerm = BetacodeTerm Text
  deriving (Eq, Show, Generic, Hashable)

instance IsString BetacodeTerm where
  fromString = BetacodeTerm . fromString

{- | Same as BetacodeTerm but betacode is mapped to ascii (latin) because accents are hard...
Example betacode term: 'iakunqotrofos'
-}
newtype NormalisedTerm = NormalisedTerm {unNormalisedTerm :: Text}
  deriving (Eq, Show, Generic, Hashable, Ord)

data Entry a = Entry
  { entryTerm :: a
  , entryDefinition :: Translation
  }
  deriving (Eq, Show, Generic, Functor)

instance Binary Translation
instance Binary BetacodeTerm
instance (Binary a) => Binary (Entry a)

entriesToDictionary ::
  [Entry BetacodeTerm] ->
  M.HashMap NormalisedTerm (S.HashSet Translation)
entriesToDictionary = Prelude.foldr addItem M.empty
 where
  addItem (Entry betacodeTerm definition) =
    M.alter (upsert definition) (betacodeToNormal betacodeTerm)
  upsert definition Nothing = Just $ S.singleton definition
  upsert definition (Just defs) = Just $ S.insert definition defs

betacodeToNormalEntry ::
  Entry BetacodeTerm ->
  Entry NormalisedTerm
betacodeToNormalEntry = fmap betacodeToNormal

betacodeToNormal ::
  BetacodeTerm ->
  NormalisedTerm
betacodeToNormal (BetacodeTerm bt) = NormalisedTerm (_betacodeToNormal bt)

_betacodeToNormal :: Text -> Text
_betacodeToNormal = foldr' onlyRoman empty
 where
  onlyRoman 'a' rest = cons 'a' rest
  onlyRoman 'b' rest = cons 'b' rest
  onlyRoman 'c' rest = cons 'c' rest
  onlyRoman 'd' rest = cons 'd' rest
  onlyRoman 'e' rest = cons 'e' rest
  onlyRoman 'f' rest = cons 'f' rest
  onlyRoman 'g' rest = cons 'g' rest
  onlyRoman 'h' rest = cons 'h' rest
  onlyRoman 'i' rest = cons 'i' rest
  onlyRoman 'k' rest = cons 'k' rest
  onlyRoman 'l' rest = cons 'l' rest
  onlyRoman 'm' rest = cons 'm' rest
  onlyRoman 'n' rest = cons 'n' rest
  onlyRoman 'o' rest = cons 'o' rest
  onlyRoman 'p' rest = cons 'p' rest
  onlyRoman 'q' rest = cons 'q' rest
  onlyRoman 'r' rest = cons 'r' rest
  onlyRoman 's' rest = cons 's' rest
  onlyRoman 't' rest = cons 't' rest
  onlyRoman 'u' rest = cons 'u' rest
  onlyRoman 'v' rest = cons 'v' rest
  onlyRoman 'w' rest = cons 'w' rest
  onlyRoman 'x' rest = cons 'x' rest
  onlyRoman 'y' rest = cons 'y' rest
  onlyRoman 'z' rest = cons 'z' rest
  onlyRoman _ rest = rest
