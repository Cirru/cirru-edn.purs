module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Cirru.Edn (CirruEdn(..))

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
