module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array (any)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (sum)
import Data.Function.Memoize (memoize, memoize2)
import Data.Identity (Identity)
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (eof)
import Text.Parsing.Parser.Token (GenTokenParser, makeTokenParser)

type BagContents = Map.Map String Int
type Bag = Tuple String BagContents
type MapEnv = Map.Map String BagContents

-- Parsing
def :: GenTokenParser String Identity
def = makeTokenParser emptyDef

parseBagDeclaration :: Parser String Bag
parseBagDeclaration = do
  name <- parseBagDescriptor
  void $ def.symbol "bags contain"
  content <- try noOtherBags <|> def.commaSep1 bagContent
  void def.dot
  pure $ Tuple name (Map.fromFoldable content)
  where
  noOtherBags :: Parser String (List.List (Tuple String Int))
  noOtherBags = List.Nil <$ def.symbol "no other bags"

  bagContent :: Parser String (Tuple String Int)
  bagContent = do
    amount <- def.natural
    name <- parseBagDescriptor
    void $ def.symbol if amount == 1 then "bag" else "bags"
    pure $ Tuple name amount

  parseBagDescriptor :: Parser String String
  parseBagDescriptor = ado 
    a <- def.identifier 
    b <- def.identifier
    in a <> " " <> b

parser :: Parser String MapEnv
parser = (Array.many parseBagDeclaration <#> Map.fromFoldable) <* eof

parse :: String -> Either ParseError MapEnv
parse = flip runParser parser

-- Solution
contains :: MapEnv -> String -> String -> Boolean
contains env = memoize2 self
  where
  self name target 
    = fromMaybe false
    $ Map.lookup name env
    <#> Map.keys
    <#> any (\subBag -> subBag == target || self subBag target)

solve :: MapEnv -> Int
solve env 
  = Map.filterKeys go env # Map.size
  where 
  go key = contains' key "shiny gold"
  contains' = contains env

childrenCount :: MapEnv -> String -> Int 
childrenCount env = memoize self
  where
  self target = sum $ uncurry go <$> content 
    where
    go name quantity = quantity * (1 + self name)

    content :: Array _
    content = Map.toUnfoldable $ fromMaybe Map.empty (Map.lookup target env)

solve' :: MapEnv -> Int
solve' env = childrenCount env "shiny gold" 

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  -- logShow $ solve <$> parse content  
  logShow $ solve' <$> parse content  
