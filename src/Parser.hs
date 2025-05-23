module Parser (parse, parseAndRest, Token (..)) where

import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)

import qualified Data.Map as Map

data Token = A | B | AA | AAA
  deriving (Show, Eq)

parse :: Map.Map String Token -> String -> [[Token]]
parse m input = map fst $ parse' m [([], input)]

parseAndRest :: Map.Map String Token -> String -> [([Token], String)]
parseAndRest terms input = parse' terms [([], input)]

{- | Essentially does the Cartesian product between the terms in the dictionary and
 the results so far until all the remainders to parse are consumed
-}
parse' ::
  -- | dictionary terms
  Map.Map String Token ->
  -- | possible results so far (history)
  [([Token], String)] ->
  -- | new results so far and the rest of the unconsumed input
  [([Token], String)]
parse' terms = go
 where
  go history | noMoreRemaindersLeft history = history -- TODO: make more efficient
  go history = go $ concat $ Map.elems $ Map.mapWithKey (parseKeywordN history) terms

noMoreRemaindersLeft :: [([Token], String)] -> Bool
noMoreRemaindersLeft = all f
 where
  f (_, "") = True
  f _ = False

parseKeywordN ::
  -- | possible results so far (history)
  [([Token], String)] ->
  -- | the term we're trying to recognise
  String ->
  -- | the token to produce if recognised
  Token ->
  -- | new results so far and the rest of the unconsumed input
  [([Token], String)]
parseKeywordN history prefix symbol = mapMaybe (parseKeyword prefix symbol) history

{- | Assume that the prefix is not empty.
>>> parseKeyword "a" A ([], "abc")
Just ([A],"bc")
>>> parseKeyword "a" A ([], "")
Nothing
>>> parseKeyword "a" A ([], "b")
Nothing
-}
parseKeyword ::
  String ->
  Token ->
  ([Token], String) ->
  Maybe ([Token], String)
parseKeyword "" _ _ = error "the prefix should not be empty!"
-- parseKeyword _ _ (symbols, "") = Just (symbols, "")
parseKeyword prefix newSymbol (symbols, input) = do
  rest <- stripPrefix prefix input
  pure (newSymbol : symbols, rest)
