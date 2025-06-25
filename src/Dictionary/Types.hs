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

import qualified Data.Serialize as Ser

newtype Translation = Translation Text
  deriving (Eq, Show, Generic, Hashable)

-- | Term containing betacode content
newtype BetacodeTerm = BetacodeTerm Text
  deriving (Eq, Show, Generic)

-- | Same as BetacodeTerm but betacode is mapped to ascii (latin) because accents are hard...
newtype NormalisedTerm = NormalisedTerm Text
  deriving (Eq, Show, Generic, Hashable)

data Entry a = Entry a Translation
  deriving (Eq, Show, Generic, Functor)

instance Ser.Serialize Translation
instance Ser.Serialize BetacodeTerm
instance (Ser.Serialize a) => Ser.Serialize (Entry a)
