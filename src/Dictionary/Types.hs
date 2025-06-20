module Dictionary.Types (
  Entry (..),
  Translation (..),
  Term (..),
  NormalisedTerm (..),
)
where

import Common
import Data.Serialize.Text ()
import Data.Text

import qualified Data.Serialize as Ser

newtype Translation = Translation Text
  deriving (Eq, Show, Generic)

-- | Term containing betacode content
newtype Term = Term Text
  deriving (Eq, Show, Generic)

-- | Same as Term but betacode is mapped to ascii (latin) because accents are hard...
newtype NormalisedTerm = NormalisedTerm Text
  deriving (Eq, Show, Generic)

data Entry a = Entry a Translation
  deriving (Eq, Show, Generic)

instance Ser.Serialize Translation
instance Ser.Serialize Term
instance (Ser.Serialize a) => Ser.Serialize (Entry a)
