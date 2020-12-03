module Main where

import Prelude

import Data.Array (head, length, mapMaybe, (!!))
import Data.BigInt as BigInt
import Data.Enum (fromEnum)
import Data.Foldable (product)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Data.Typelevel.Num (D2, d0, d1)
import Data.Vec (Vec, vec2)
import Data.Vec as Vec
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type CharArray = Array Char

type MapData = Boolean
type TreeMatrix = Array (Array Boolean)
type Vec2 = Vec D2 Int
type TreeMap = Vec2 -> Maybe MapData

-- Parsing
parseLine :: CharArray -> Array MapData
parseLine chars = flip mapMaybe chars case _ of
  '#' -> Just true
  '.' -> Just false
  _ -> Nothing

parseMap :: String -> TreeMatrix
parseMap s = lines s <#> toCharArray <#> parseLine

-- Solution
translate :: Vec2 -> TreeMap -> TreeMap
translate vec lookup position = lookup (vec + position) 

lookupMatrix :: TreeMatrix -> Vec2 -> Maybe MapData 
lookupMatrix m position = do
  width <- length <$> head m
  line <- m !! y
  line !! (x `mod` width)
  where
  x = Vec.index position d0 
  y = Vec.index position d1

unfoldPosition :: Vec2 -> TreeMap -> Int
unfoldPosition slope = go
  where 
  go lookup = case lookup slope of
    Nothing -> 0
    Just isTree -> go (translate slope lookup) + fromEnum isTree

slopes :: Array Vec2
slopes =
  [ vec2 1 1
  , vec2 3 1
  , vec2 5 1
  , vec2 7 1
  , vec2 1 2 ]

solve :: TreeMap -> BigInt.BigInt
solve treeMap = bigProduct $ go <$> slopes 
  where
  bigProduct = map BigInt.fromInt >>> product
  go slope = unfoldPosition slope treeMap

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  let slice = lookupMatrix $ parseMap content
  logShow $ solve slice
