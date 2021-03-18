module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Cirru.Edn (CirruEdn(..), parseCirruEdn)

main :: Effect Unit
main = do
  log "🍝"
  d <- parseCirruEdn "quote $ demo"
  log $ show d
  d <- parseCirruEdn "do true"
  log $ show d
  log "You should add some tests."
