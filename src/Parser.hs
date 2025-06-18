module Parser (parse) where

import Data.List (stripPrefix)

import qualified Data.Map as Map

-- Given a map of terms and symbols, generate all possible parsings of the given string, the
-- map defines a regular language
parse :: Map.Map String result -> String -> [[result]]
parse dict = map snd . just (many $ dictionary dict)

-- Keeps only results that no longer present any remaining input
just :: Parser symbol -> Parser symbol
just p = filter ((==) "" . fst) . p

type Parser result = String -> [(String, result)]

-- epsilon :: Parser Char ()
-- epsilon xs = [(xs, ())]

-- fail :: Parser Char r
-- fail _ = []

succeed :: r -> Parser r
succeed v xs = [(xs, v)]

keywordToken :: String -> symbol -> Parser symbol
keywordToken prefix newSymbol xs =
  case stripPrefix prefix xs of
    Just rest -> [(rest, newSymbol)]
    Nothing -> []

(<|>) :: Parser a -> Parser a -> Parser a
(p1 <|> p2) xs = p1 xs ++ p2 xs

(<*>) :: (Monoid a) => Parser a -> Parser a -> Parser a
(p1 <*> p2) xs =
  [ (xs2, v1 <> v2)
  | (xs1, v1) <- p1 xs
  , (xs2, v2) <- p2 xs1
  ]

many :: (Monoid symbol) => Parser symbol -> Parser symbol
many p = (p Parser.<*> many p) Parser.<|> succeed mempty

alts :: [Parser symbol] -> Parser symbol
alts = foldl (Parser.<|>) mempty

dictionaryToAlts :: Map.Map String symbol -> [Parser symbol]
dictionaryToAlts = map (uncurry keywordToken) . Map.toList

dictionary :: Map.Map String symbol -> Parser [symbol]
dictionary = alts . dictionaryToAlts . Map.map pure
