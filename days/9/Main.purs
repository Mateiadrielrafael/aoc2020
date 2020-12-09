module Main where

import Prelude

import Data.Array (mapMaybe)
import Data.Foldable (maximum, minimum)
import Data.Int (fromString)
import Data.List (List(..), (:), (!!))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

preambleLength :: Int
preambleLength = 25

-- Parsing
parse :: String -> List Int
parse = lines >>> mapMaybe fromString >>> List.fromFoldable

-- Solution
isSumOf :: Int -> List Int -> Boolean
isSumOf target = go Set.empty
  where
  go visited Nil = false
  go visited (head:tail) | Set.member (target - head) visited = true
                         | otherwise = go (Set.insert head visited) tail

solve :: List Int -> Maybe Int
solve numbers@(_:tail) | Just current <- numbers !! preambleLength 
  = if isSumOf current $ List.take preambleLength numbers 
    then solve tail 
    else Just current
solve _ = Nothing    

findContiguousSum :: List Int -> Int -> Maybe (List Int)
findContiguousSum Nil 0 = Just Nil
findContiguousSum Nil _ = Nothing
findContiguousSum numbers@(_:tail) target = case go target numbers of
  Just length -> Just $ List.take length numbers
  Nothing -> findContiguousSum tail target
  where
  go 0 _ = Just 0
  go _ Nil = Nothing
  go max (head:tail') | max >= head = go (max - head) tail' <#> ((+) 1)
                      | otherwise = Nothing

solve' :: List Int -> Int -> Maybe Int 
solve' numbers target = (+) <$> (sum >>= maximum) <*> (sum >>= minimum) 
  where
  sum = findContiguousSum numbers target 

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  let
    numbers = parse content 
    broken = solve numbers
  logShow broken
  logShow $ broken >>= solve' numbers
