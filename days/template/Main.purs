module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  logShow "toSolve"