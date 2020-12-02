module Main where

import Prelude

import Data.Array (catMaybes, filter, length)
import Data.Array as Array
import Data.Either (hush)
import Data.Function (on)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String.CodeUnits as CodeUnits
import Data.String.Utils (toCharArray)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), GenTokenParser, letter, makeTokenParser, unGenLanguageDef)

type PasswordPolicy = { min :: Int, max :: Int, char :: String }

-- Parsing
def :: GenTokenParser String Identity
def = makeTokenParser $ LanguageDef (unGenLanguageDef emptyDef)
  { identStart      = letter
  , identLetter     = letter
  , reservedOpNames = ["-", ":"]
  , opStart = oneOf ['-', ':']
  , opLetter = oneOf ['-', ':']
  }

policy :: Parser String PasswordPolicy
policy = do
  min <- def.integer
  def.reservedOp "-"
  max <- def.integer 
  char <- letter
  pure { min, max, char: CodeUnits.singleton char }

line :: Parser String (Tuple PasswordPolicy String)
line = do
  currentPolicy <- policy
  def.reservedOp ":"
  password <- def.identifier
  pure $ Tuple currentPolicy password

parse :: String -> (Array (Tuple PasswordPolicy String))
parse =  catMaybes <<< map (hush <<< flip runParser line) <<< split (Pattern "\n") 

-- Solution
validPassword :: PasswordPolicy -> String -> Boolean
validPassword { min, max, char } s = l >= min && l <= max
  where l = length $ filter ((==) char) $ toCharArray s 

validPassword' :: PasswordPolicy -> String -> Boolean
validPassword' { min, max, char } s = on notEq (\i -> Just char == Array.index charArray (i - 1)) min max
  where
  charArray = toCharArray s

solve :: (PasswordPolicy -> String -> Boolean) -> Array (Tuple PasswordPolicy String) -> Int
solve validator = length <<< filter (uncurry validator)

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  logShow $ solve validPassword' (parse content)