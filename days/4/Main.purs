module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.MonadZero (empty, guard)
import Data.Array (filter, fold, foldr, length)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either, isRight)
import Data.Enum (enumFromTo)
import Data.HashMap (HashMap)
import Data.HashMap as Map
import Data.Identity (Identity)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (char, eof, oneOf, string)
import Text.Parsing.Parser.Token (GenTokenParser, alphaNum, digit, makeTokenParser)

type Passport = HashMap String String

-- Validators with semigroup instances
newtype FieldValidator = FieldValidator (Passport -> Boolean)

instance filedValidatorSemigroup :: Semigroup FieldValidator where
  append (FieldValidator a) (FieldValidator b) = FieldValidator \input -> a input && b input

instance fieldValidatorMonoid :: Monoid FieldValidator where
  mempty = FieldValidator (const true)

derive instance fieldValidatorNewtype :: Newtype FieldValidator _ 

-- Helpers for validating
validateKey :: String -> (String -> Boolean) -> FieldValidator
validateKey key validate = FieldValidator \input -> case Map.lookup key input of
  Just value -> validate value
  Nothing -> false

validateInt :: (Int -> Boolean) -> (String -> Boolean)
validateInt f input = case fromString input of
  Just value -> f value
  Nothing -> false

withParser :: forall a. Parser String a -> (String -> Boolean)
withParser parser = flip runParser parser >>> isRight

-- Utils
count :: forall a. Eq a =>  a -> Array a -> Int
count el arr = length $ filter (eq el) arr 

-- Parsing
def :: GenTokenParser String Identity
def = makeTokenParser emptyDef

identifier :: Parser String String
identifier = ado
  chars <- Array.some $ alphaNum <|> oneOf [ '#' ]
  in fromCharArray chars
    
whiteSpace :: Parser String (NonEmptyArray.NonEmptyArray Char)
whiteSpace = NonEmptyArray.some (oneOf [' ', '\n'])

parsePair :: Parser String (Tuple String String)
parsePair = ado
  name <- identifier
  void $ char ':'
  value <- identifier
  in Tuple name value
  
parsePassport :: Parser String Passport
parsePassport = fix \self -> do
  (Tuple key value) <- parsePair
  ws <- NonEmptyArray.toArray <$> whiteSpace 
  if count '\n' ws < 2 
    then ado 
      next <- (Map.empty <$ eof) <|> self
      in Map.insert key value next
    else pure $ Map.singleton key value

parse :: String -> Either ParseError (Array Passport)
parse input = runParser input parser
  where
  parser = Array.many parsePassport

-- solve
validPassport :: Passport -> Boolean
validPassport passport = 7 == Map.size withoutCid 
  where
  withoutCid = Map.filterKeys ((/=) "cid") passport

validatePassport' :: Passport -> Boolean
validatePassport' = unwrap $ fold [byr, iyr, eyr, hgt, hcl, ecl, pid]
  where
  byr = validateKey "byr" $ validateInt \year -> year >= 1920 && year <= 2002
  iyr = validateKey "iyr" $ validateInt \year -> year >= 2010 && year <= 2020
  eyr = validateKey "eyr" $ validateInt \year -> year >= 2020 && year <= 2030

  hgt = validateKey "hgt" $ withParser do
    num <- def.natural
    uom <- string "in" <|> string "cm"
    guard if uom == "cm"
      then 150 <= num && num <= 193
      else 59 <= num && num <= 76

  hcl = validateKey "hcl" $ withParser do
    void $ char '#'
    chars <- Array.many $ digit <|> oneOf (enumFromTo 'a' 'f')
    guard $ length chars == 6

  ecl = validateKey "ecl" $ withParser 
    $ foldr (<|>) empty $ string <$> eyeColors
    where
    eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

  pid = validateKey "pid" $ withParser do
    digits <- Array.many digit
    guard $ length digits == 9

solve :: (Passport -> Boolean) -> Array Passport -> Int
solve validate = map validate >>> filter identity >>> length

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  logShow 
    -- $ solve validatePassport <$> parse content 
    $ solve validatePassport' <$> parse content