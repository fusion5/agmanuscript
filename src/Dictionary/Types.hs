module Dictionary.Types (
  Entry (..),
  Translation (..),
  BetacodeTerm (..),
  NormalisedTerm (..),
)
where

import Common
import Data.Serialize.Text ()
import Data.Text
import Data.Hashable
import Data.String
import Data.Binary

import qualified Data.Serialize as Ser

newtype Translation = Translation Text
  deriving (Eq, Show, Generic, Hashable)

-- | Term containing betacode content. Example betacode word:
-- `*(iakunqotro/fos`
newtype BetacodeTerm = BetacodeTerm Text
  deriving (Eq, Show, Generic, Hashable)

instance IsString BetacodeTerm where
  fromString = BetacodeTerm . fromString

-- | Same as BetacodeTerm but betacode is mapped to ascii (latin) because accents are hard...
-- Example betacode term: 'iakunqotrofos'
newtype NormalisedTerm = NormalisedTerm Text
  deriving (Eq, Show, Generic, Hashable)

data Entry a = Entry a Translation
  deriving (Eq, Show, Generic, Functor)

instance Binary Translation
instance Binary BetacodeTerm
instance Binary a => Binary (Entry a)

instance Ser.Serialize Translation
instance Ser.Serialize BetacodeTerm
instance (Ser.Serialize a) => Ser.Serialize (Entry a)
