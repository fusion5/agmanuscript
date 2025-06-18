module Parser (parse) where

import Data.List (stripPrefix)

import qualified Data.Map as Map

-- Given a map of terms and symbols, generate all possible parsings of the given string, the
-- map defines a regular language
parse :: Map.Map String result -> String -> [[result]]
parse dict = map snd . just (many $ dictionary dict)

-- Keeps only results that no longer present any remaining input
just :: Parser Char a -> Parser Char a
just p = filter ((==) "" . fst) . p

type Parser symbol result = [symbol] -> [([symbol], result)]

-- epsilon :: Parser Char ()
-- epsilon xs = [(xs, ())]

-- fail :: Parser Char r
-- fail _ = []

succeed :: r -> Parser Char r
succeed v xs = [(xs, v)]

keywordToken :: String -> a -> Parser Char a
keywordToken prefix newSymbol xs =
  case stripPrefix prefix xs of
    Just rest -> [(rest, newSymbol)]
    Nothing -> []

(<|>) :: Parser s a -> Parser s a -> Parser s a
(p1 <|> p2) xs = p1 xs ++ p2 xs

(<*>) :: (Monoid a) => Parser s a -> Parser s a -> Parser s a
(p1 <*> p2) xs =
  [ (xs2, v1 <> v2)
  | (xs1, v1) <- p1 xs
  , (xs2, v2) <- p2 xs1
  ]

many :: (Monoid a) => Parser Char a -> Parser Char a
many p = (p Parser.<*> many p) Parser.<|> succeed mempty

alts :: [Parser Char a] -> Parser Char a
alts = foldl (Parser.<|>) mempty

dictionaryToAlts :: Map.Map String a -> [Parser Char a]
dictionaryToAlts = map (uncurry keywordToken) . Map.toList

dictionary :: Map.Map String symbol -> Parser Char [symbol]
dictionary = alts . dictionaryToAlts . Map.map pure
