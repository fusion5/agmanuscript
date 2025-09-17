module Dictionary (convertEntries) where

import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Dictionary.BetaConv
import Dictionary.Types

convertEntries ::
  [Entry BetacodeTerm] ->
  M.HashMap NormalisedTerm (S.HashSet Translation)
convertEntries = Prelude.foldr addItem M.empty
 where
  addItem (Entry betacodeTerm definition) =
    M.alter (upsert definition) (betacodeToNormal betacodeTerm)
  upsert definition Nothing = Just $ S.singleton definition
  upsert definition (Just defs) = Just $ S.insert definition defs
