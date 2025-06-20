module Dictionary.BetaConv (
  betacodeToRoman,
) where

import Data.Text

betacodeToRoman :: Text -> Text
betacodeToRoman = foldr' (flip onlyRoman) empty
  where
    onlyRoman rest 'a' = cons 'a' rest
    onlyRoman rest 'b' = cons 'b' rest
    onlyRoman rest 'c' = cons 'c' rest
    onlyRoman rest 'd' = cons 'd' rest
    onlyRoman rest 'e' = cons 'e' rest
    onlyRoman rest 'f' = cons 'f' rest
    onlyRoman rest 'g' = cons 'g' rest
    onlyRoman rest 'h' = cons 'h' rest
    onlyRoman rest 'i' = cons 'i' rest
    onlyRoman rest 'k' = cons 'k' rest
    onlyRoman rest 'l' = cons 'l' rest
    onlyRoman rest 'm' = cons 'm' rest
    onlyRoman rest 'n' = cons 'n' rest
    onlyRoman rest 'o' = cons 'o' rest
    onlyRoman rest 'p' = cons 'p' rest
    onlyRoman rest 'q' = cons 'q' rest
    onlyRoman rest 'r' = cons 'r' rest
    onlyRoman rest 's' = cons 's' rest
    onlyRoman rest 't' = cons 't' rest
    onlyRoman rest 'u' = cons 'u' rest
    onlyRoman rest 'v' = cons 'v' rest
    onlyRoman rest 'w' = cons 'w' rest
    onlyRoman rest 'x' = cons 'x' rest
    onlyRoman rest 'y' = cons 'y' rest
    onlyRoman rest 'z' = cons 'z' rest
    onlyRoman rest _   = rest
