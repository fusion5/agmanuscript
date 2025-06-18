module Parser (parse, Token (..)) where

import Data.List (stripPrefix)

import qualified Data.Map as Map

data Token = A | B | AA | AAA
  deriving (Show, Eq)

parse :: Map.Map String [Token] -> String -> [[Token]]
parse dict = map snd . just (many $ dictionary dict)

-- Keeps only results that no longer present any remaining input
just :: Parser Char a -> Parser Char a
just p = filter f . p
 where
  f ("", _) = True
  f _ = False

type Parser symbol result = [symbol] -> [([symbol], result)]

-- epsilon :: Parser Char ()
-- epsilon xs = [(xs, ())]

succeed :: r -> Parser Char r
succeed v xs = [(xs, v)]

-- fail :: Parser Char r
-- fail _ = []

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

dictionary :: Map.Map String [Token] -> Parser Char [Token]
dictionary = alts . dictionaryToAlts

-- >>> _test4 "aab"
-- [("",[A,A,B]),("b",[A,A]),("ab",[A]),("",[AA,B]),("b",[AA]),("aab",[])]
_test4 :: Parser Char [Token]
_test4 = many (dictionary testTerms)
 where
  testTerms :: Map.Map String [Token]
  testTerms =
    Map.fromList
      [ ("a", pure A)
      , ("aa", pure AA)
      , ("aaa", pure AAA)
      , ("b", pure B)
      ]

-- >>> test1 "ab"
-- [("",[A,B])]
_test1 :: Parser Char [Token]
_test1 = keywordToken "a" [A] Parser.<*> keywordToken "b" [B]

-- >>> test2 "a"
-- [("",[A])]
-- >>> test2 "b"
-- [("",[B])]
_test2 :: Parser Char [Token]
_test2 = keywordToken "a" [A] Parser.<|> keywordToken "b" [B]

-- >>> test3 "aab"
-- [("",[A,A,B]),("b",[A,A]),("ab",[A]),("aab",[])]
_test3 :: Parser Char [Token]
_test3 = many _test2
