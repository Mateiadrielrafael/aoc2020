module Main where

import Prelude

import Control.MonadZero (guard)
import Data.Array (head, mapMaybe)
import Data.Int (fromString)
import Data.Maybe (Maybe)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

solve :: Array Int -> Maybe Int
solve entries = head do
  a <- entries
  b <- entries
  c <- entries
  guard (a + b + c == 2020)
  pure $ a * b * c

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"  
  let numbers = mapMaybe fromString $ lines content
  logShow $ solve numbers
