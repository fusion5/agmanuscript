module Dictionary.BetaConv (
  betacodeToNormal,
  betacodeToNormalEntry,
) where

import Data.Text
import Dictionary.Types

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
