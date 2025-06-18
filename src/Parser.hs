module Parser (parse) where

import Data.List (stripPrefix)

import qualified Data.Map as Map

type Parser result = String -> [(String, result)]

-- Given a map of terms and symbols, generate all possible parsings of the given string (the
-- map defines a regular language of a repeating sequence of terms)
parse :: Map.Map String result -> String -> [[result]]
parse dict = map snd . just (many $ dictionary dict)

-- Keeps only results that no longer present any remaining input
just :: Parser symbol -> Parser symbol
just p = filter ((==) "" . fst) . p

succeed :: r -> Parser r
succeed v xs = [(xs, v)]

keywordToken :: String -> symbol -> Parser symbol
keywordToken prefix newSymbol xs =
  case stripPrefix prefix xs of
    Just rest -> [(rest, newSymbol)]
    Nothing -> []

alternative :: Parser a -> Parser a -> Parser a
alternative p1 p2 xs = p1 xs ++ p2 xs

-- | 0..n occurences of the parser
many :: Parser symbol -> Parser [symbol]
many p = (p `parserSequencing` many p) `alternative` succeed []
 where
  parserSequencing :: Parser a -> Parser [a] -> Parser [a]
  parserSequencing p1 p2 xs =
    [ (xs2, v1 : v2)
    | (xs1, v1) <- p1 xs
    , (xs2, v2) <- p2 xs1
    ]

alts :: [Parser symbol] -> Parser symbol
alts = foldl alternative mempty

dictionaryToAlts :: Map.Map String symbol -> [Parser symbol]
dictionaryToAlts = map (uncurry keywordToken) . Map.toList

dictionary :: Map.Map String symbol -> Parser symbol
dictionary = alts . dictionaryToAlts
