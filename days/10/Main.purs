module Main where

import Prelude

import Data.Array (mapMaybe)
import Data.Bifunctor (lmap, rmap)
import Data.BigInt as BigInt
import Data.Foldable (maximum, sum)
import Data.Int (fromString)
import Data.List (List(..), length, take, takeWhile, (:))
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

-- Parsing
parse :: String -> List Int
parse = lines >>> mapMaybe fromString >>> List.fromFoldable >>> addDeviceJoltage  >>> List.sort 
  where
  addDeviceJoltage list = List.Cons (3 + fromMaybe 0 (maximum list)) list

-- Histomoprhism implementation for lists
-- Welp, it's a little bit more than a histomoprhism, 
-- took inspiration from paramorphisms to allow the function to access the whole list at any point
histo :: forall a b. (List b -> List a -> a -> b -> b) -> b -> List a -> b
histo f d l = snd $ histo' f d l

histo' :: forall a b. (List b -> List a -> a -> b -> b) -> b -> List a -> Tuple (List b) b
histo' f d Nil = Tuple Nil d
histo' f d (head:tail) = Tuple (current:all) current 
  where
  (Tuple all last) = histo' f d tail
  current = f all tail head last

-- Solution
type DifferenceCount = Tuple Int Int

solve :: List Int -> Int
solve input = fst differences * snd differences  
  where
  differences = countDifferences input

  countDifferences (head:tail) = increaseCount head $ countDifferences $ (_ - head) <$> tail
  countDifferences _ = Tuple 0 0 

  increaseCount :: Int -> DifferenceCount -> DifferenceCount
  increaseCount 1 = lmap ((+) 1)
  increaseCount 3 = rmap ((+) 1)
  increaseCount _ = identity 

solve' :: List Int -> BigInt.BigInt
solve' input = histo go one (0:input)
  where
  go Nil _ _ _ = one
  go caseCounts adapters current count = sum $ take (countSkippable current adapters) caseCounts

  countSkippable :: Int -> List Int -> Int
  countSkippable power = takeWhile (\e -> e - power <= 3) >>> length

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  logShow $ solve $ parse content
  logShow $ solve' $ parse content
  