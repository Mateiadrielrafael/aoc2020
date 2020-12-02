module Main where

import Prelude

import Data.Array (catMaybes, filter, length)
import Data.Array as Array
import Data.Either (hush)
import Data.Function (on)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.Token (GenTokenParser, letter, makeTokenParser)

type PasswordPolicy = { min :: Int, max :: Int, char :: Char }
type Password = Tuple PasswordPolicy (Array Char)
type Validator = PasswordPolicy -> Array Char -> Boolean

-- Parsing
def :: GenTokenParser String Identity
def = makeTokenParser emptyDef

policy :: Parser String PasswordPolicy
policy = ado
  min <- def.natural
  def.reservedOp "-"
  max <- def.natural 
  char <- letter
  in { min, max, char }

line :: Parser String Password
line = ado
  currentPolicy <- policy
  def.reservedOp ":"
  password <- def.identifier
  in Tuple currentPolicy (toCharArray password)

parse :: String -> Array Password
parse = catMaybes <<< map (hush <<< flip runParser line) <<< lines

-- Solution
validPassword :: Validator
validPassword { min, max, char } chars = l >= min && l <= max
  where l = length $ filter ((==) char) chars

validPassword' :: Validator
validPassword' { min, max, char } chars = on notEq (\i -> Just char == Array.index chars (i - 1)) min max

solve :: Validator -> Array Password -> Int
solve validator = length <<< filter (uncurry validator) 

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  logShow $ solve validPassword' (parse content)