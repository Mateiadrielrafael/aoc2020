module Main where

import Prelude

import Control.MonadZero (guard)
import Data.Array (catMaybes, elem, filter, length, mapMaybe, mapWithIndex, (!!))
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

data Seat = Occupied | Unoccupied | Floor 

type Board = Array (Array Seat)
type Point = Vec D2 Int

-- Parsing
parse :: String -> Board
parse s = lines s <#> toCharArray <#> mapMaybe case _ of
  '#' -> Just Unoccupied
  'L' -> Just Occupied
  '.' -> Just Floor
  _ -> Nothing

-- Solution
count :: forall a. Eq a => a -> Array a -> Int
count el = filter ((==) el) >>> length

lookupBoard :: Board -> Point -> Maybe Seat
lookupBoard board position = board !! y >>= (_ !! x)
  where
  x = position `Vec.index` d0
  y = position `Vec.index` d1

getClosestSeat :: Board -> Point -> Point -> Maybe Seat
getClosestSeat board direction position 
  = lookupBoard board position'  
  >>= case _ of
    Floor -> getClosestSeat board direction position' 
    x -> Just x
  where
  position' = position + direction

adjacentSeats :: Board -> Point -> Array Seat
adjacentSeats board position = catMaybes do
  m <- [-1, 0, 1]
  n <- [-1, 0, 1]
  guard $ m /= 0 || n /= 0
  pure $ getClosestSeat board (vec2 m n) position

advance :: Seat -> Array Seat -> Seat 
advance Unoccupied adjacent | not $ elem Occupied adjacent = Occupied
advance Occupied adjacent | count Occupied adjacent >= 5 = Unoccupied
advance x _ = x

advanceBoard :: Board -> Board
advanceBoard board = flip mapWithIndex board \y arr -> flip mapWithIndex arr \x seat -> next x y seat 
  where
  next x y seat = advance seat $ adjacentSeats board (vec2 x y)

solve :: Board -> Int
solve board = if board' == board then count Occupied (join board') else solve board' 
  where
  board' = advanceBoard board

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  logShow $ solve $ parse content

-- typeclass instances
derive instance eqSeat :: Eq Seat