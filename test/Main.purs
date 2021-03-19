module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Data.Either (Either(..))

import Cirru.Node (CirruNode(..))
import Cirru.Edn (parseCirruEdn, CirruEdn(..), CrEdnKv(..))

import Test.Unit (suite, test, timeout)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

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

  case (parseCirruEdn "%{} P (name age) (|Chen 20)") of
    Right d -> log $ show d
    Left failure -> log $ "Failed at: " <> (show failure)

  runTest do
    suite "parse code" do
      test "parse quote" do
        Assert.equal (parseCirruEdn "quote $ demo") (Right $ CrEdnQuote $ CirruList [CirruLeaf "demo"])
      test "parse boolean" do
        Assert.equal (parseCirruEdn "do true") (Right $ CrEdnBool true)
      test "parse list" do
        Assert.equal (parseCirruEdn "[] true true true")
          (Right $ CrEdnList [CrEdnBool true, CrEdnBool true, CrEdnBool true])

      test "parse nested list" do
        Assert.equal (parseCirruEdn "[] true true $ [] true")
          (Right $ CrEdnList [CrEdnBool true, CrEdnBool true, CrEdnList [CrEdnBool true]])

      test "parse map" do
        Assert.equal (parseCirruEdn "{} (:a 1) (:b 2)")
          (Right $ CrEdnMap [CrEdnKv (CrEdnKeyword "a") (CrEdnNumber 1.0),
                             CrEdnKv (CrEdnKeyword "b") (CrEdnNumber 2.0) ])

      test "parse record" do
        Assert.equal (parseCirruEdn "%{} P (name age) (|Chen 20)")
          (Right $ CrEdnRecord "P" ["name", "age"] [CrEdnString "Chen", CrEdnNumber 20.0])
