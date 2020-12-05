module Main where

import Prelude

import Data.Array (find, mapMaybe)
import Data.Foldable (maximum)
import Data.Int.Parse (parseInt, toRadix)
import Data.Maybe (Maybe)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.CodeUnits (drop, take)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type SeatPosition = Tuple Int Int

-- Parsing
parseSeat :: String -> Maybe SeatPosition 
parseSeat s = ado 
  y <- parseBinary (take 7 binary)
  x <- parseBinary (drop 7 binary)
  in Tuple y x  
  where
  parseBinary = flip parseInt (toRadix 2)
  binary 
    = replaceAll (Pattern "F") _0 
    $ replaceAll (Pattern "B") _1 
    $ replaceAll (Pattern "R") _1 
    $ replaceAll (Pattern "L") _0 s
  _0 = Replacement "0"
  _1 = Replacement "1"

parse :: String -> Array SeatPosition
parse = lines >>> mapMaybe parseSeat

-- Solution
getId :: SeatPosition -> Int
getId (Tuple y x) = y * 8 + x

solve :: Array SeatPosition -> Maybe Int
solve = map getId >>> maximum 

solve' :: Array SeatPosition -> Maybe Int
solve' seats = find checkSeat ids <#> ((+) 1) 
  where
  checkSeat seatId = Set.member (seatId + 2) ids && not (Set.member (seatId + 1) ids)
  ids = Set.fromFoldable $ getId <$> seats

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  logShow $ solve' $ parse content