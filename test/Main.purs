module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Data.Either (Either(..))

import Cirru.Edn (CirruEdn(..), parseCirruEdn, CrEdnParsed)

main :: Effect Unit
main = do
  case (parseCirruEdn "quote $ demo") of
    Right d -> log $ show d
    Left failure -> log $ "Failed at: " <> (show failure)
  case (parseCirruEdn "do true") of
    Right d -> log $ show d
    Left failure -> log $ "Failed at: " <> (show failure)
  case (parseCirruEdn "[] true true true") of
    Right d -> log $ show d
    Left failure -> log $ "Failed at: " <> (show failure)

  case (parseCirruEdn "[] true true $ [] true") of
    Right d -> log $ show d
    Left failure -> log $ "Failed at: " <> (show failure)

  case (parseCirruEdn "#{} true true $ [] true") of
    Right d -> log $ show d
    Left failure -> log $ "Failed at: " <> (show failure)

  case (parseCirruEdn "{} (:a 1) (:b 2)") of
    Right d -> log $ show d
    Left failure -> log $ "Failed at: " <> (show failure)
