module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.List (filter, range)

main :: Effect Unit
main = do
  --log "Hello sailor!"
  log ("The answer is " <> show result)

numbers = range 1 100

multiple n = mod n 3 == 0 || mod n 5 == 0

result = filter multiple numbers
