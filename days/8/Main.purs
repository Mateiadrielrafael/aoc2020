module Main where

import Prelude

import Control.Monad.State (State, modify, runState)
import Data.Array (catMaybes, filter, mapMaybe, mapWithIndex, (!!))
import Data.Array as Array
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String.Utils (lines, words)
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

data Instruction = Noop Int | Acc Int | Jump Int

-- Parsing
parseInstruction :: String -> Maybe Instruction
parseInstruction = words >>> case _ of
  [command, amount] -> instruction command <*> fromString amount
  _ -> Nothing

  where
  instruction "nop" = Just Noop
  instruction "acc" = Just Acc
  instruction "jmp" = Just Jump
  instruction _ = Nothing

parse :: String -> Array Instruction
parse = lines >>> mapMaybe parseInstruction

-- Solution
interpret :: Array Instruction -> Tuple Boolean Int
interpret instructions = runState (go Set.empty 0) 0
  where 
  go :: Set.Set Int -> Int -> State Int Boolean
  go visited index = fromMaybe (pure true) ado 
    instruction <- instructions !! index
    let visited' = Set.insert index visited
    in case instruction of
      Noop _ -> go visited' (index + 1)
      Jump amount | Set.member (index + amount) visited' -> pure false
                  | otherwise -> go visited' (index + amount)
      Acc amount -> go visited' (index + 1) <* modify ((+) amount)

solve' :: Array Instruction -> Maybe Int
solve' instructions = map snd $ Array.head $ filter fst $ interpreted 
  where
  interpreted :: Array (Tuple Boolean Int)
  interpreted = replaced <#> interpret 

  replaced :: Array (Array Instruction)
  replaced = catMaybes $ mapWithIndex replace instructions
   
  replace :: Int -> Instruction -> Maybe (Array Instruction)
  replace index value = do 
    value' <- case value of
      Noop amount -> Just $ Jump amount
      Jump amount -> Just $ Noop amount
      _ -> Nothing
    Array.updateAt index value' instructions

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  let instructions = parse content
  logShow $ interpret instructions
  logShow $ solve' instructions