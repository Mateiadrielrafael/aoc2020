module Main where

import Prelude

import Data.Array (head, length, mapMaybe, (!!))
import Data.BigInt as BigInt
import Data.Enum (fromEnum)
import Data.Foldable (product)
import Data.Maybe (Maybe(..), maybe)
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
type TreeMap = Array (Array Boolean)
type Vec2 = Vec D2 Int
type TreeMapSlice = Vec2 -> Maybe MapData

-- Utility stuff
mapWidth :: TreeMap -> Int
mapWidth = maybe 0 length <<< head

translate :: Vec2 -> TreeMapSlice -> TreeMapSlice
translate vec lookup position = lookup (vec + position) 

toSlice :: TreeMap -> TreeMapSlice 
toSlice m position = m !! y >>= (_ !! x)
  where
  x = Vec.index position d0 `mod` width
  y = Vec.index position d1
  width = mapWidth m

-- Parsing
parseLine :: CharArray -> Array MapData
parseLine chars = flip mapMaybe chars case _ of
  '#' -> Just true
  '.' -> Just false
  _ -> Nothing

parseMap :: String -> TreeMap
parseMap s = lines s <#> toCharArray <#> parseLine

-- Solution
unfoldPosition :: Vec2 -> TreeMapSlice -> Int
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

solve :: TreeMapSlice -> BigInt.BigInt
solve treeMap = bigProduct $ slopes <#> (\slope -> unfoldPosition slope treeMap) 
  where
  bigProduct = map BigInt.fromInt >>> product

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  let slice = toSlice $ parseMap content
  logShow $ solve slice