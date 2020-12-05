module Main where

import Prelude

import Data.Array (find, mapMaybe)
import Data.Int.Parse (parseInt, toRadix)
import Data.Maybe (Maybe)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

-- Parsing
parseSeat :: String -> Maybe Int 
parseSeat s = parseInt binary (toRadix 2)  
  where
  binary 
    = replaceAll (Pattern "F") _0 
    $ replaceAll (Pattern "B") _1 
    $ replaceAll (Pattern "R") _1 
    $ replaceAll (Pattern "L") _0 s
  _0 = Replacement "0"
  _1 = Replacement "1"

parse :: String -> Array Int
parse = lines >>> mapMaybe parseSeat

-- Solution
solve :: Array Int -> Maybe Int
solve seats = find checkSeat ids <#> ((+) 1) 
  where
  checkSeat seatId = Set.member (seatId + 2) ids && not (Set.member (seatId + 1) ids)
  ids = Set.fromFoldable seats

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  logShow $ solve $ parse content