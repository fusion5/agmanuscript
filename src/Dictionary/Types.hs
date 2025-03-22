module Dictionary.Types (
  Entry (..),
  Translation (..),
)
where

import Common
import Data.Serialize.Text ()
import Data.Text

import qualified Data.Serialize as Ser

newtype Translation = Translation Text
  deriving (Eq, Show, Generic)

data Entry = Entry Text Translation
  deriving (Eq, Show, Generic)

instance Ser.Serialize Translation
instance Ser.Serialize Entry
