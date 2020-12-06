module Main where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (sum)
import Data.NonEmpty (foldl1)
import Data.Set as Set
import Data.String (Pattern(..), split, trim)
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

solve :: (Set.Set Char -> Set.Set Char -> Set.Set Char) -> String -> Int 
solve merge
  = parse 
  >>> Array.mapMaybe mergeSets 
  >>> map Set.size 
  >>> sum 
  where
  parse 
    = split (Pattern "\n\n") 
    >>> map trim 
    >>> map toGroup

  toGroup
    = lines 
    >>> map toCharArray 
    >>> map Set.fromFoldable 

  mergeSets
    = NonEmptyArray.fromFoldable 
    >>> map NonEmptyArray.toNonEmpty 
    >>> map (foldl1 merge) 

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  logShow $ solve Set.union content
  logShow $ solve Set.intersection content