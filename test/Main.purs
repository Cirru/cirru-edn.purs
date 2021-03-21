module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Data.Either (Either(..))
import Data.Tuple
import Data.Map as DataMap

import Cirru.Node (CirruNode(..))
import Cirru.Edn (parseCirruEdn, CirruEdn(..))

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
          (Right $ CrEdnMap $
            DataMap.fromFoldable [Tuple (CrEdnKeyword "a") (CrEdnNumber 1.0),
                                    Tuple (CrEdnKeyword "b") (CrEdnNumber 2.0) ])

      test "parse record" do
        Assert.equal (parseCirruEdn "%{} P (name age) (|Chen 20)")
          (Right $ CrEdnRecord "P" ["name", "age"] [CrEdnString "Chen", CrEdnNumber 20.0])

  runTest do
    suite "ordering" do
      test "compare nodes" do
        Assert.equal LT $ compare (parseCirruEdn "do false") (parseCirruEdn "do true")
        Assert.equal LT $ compare (parseCirruEdn "do nil") (parseCirruEdn "do true")
        Assert.equal LT $ compare (parseCirruEdn "do nil") (parseCirruEdn "do 1")
        Assert.equal LT $ compare (parseCirruEdn "do |string") (parseCirruEdn "[]")
        Assert.equal LT $ compare (parseCirruEdn "[]") (parseCirruEdn "#{}")
        Assert.equal LT $ compare (parseCirruEdn "[] 1 2 3") (parseCirruEdn "#{}")
        Assert.equal LT $ compare (parseCirruEdn "[] 1") (parseCirruEdn "[] 1 1")
        Assert.equal LT $ compare (parseCirruEdn "#{}") (parseCirruEdn "{}")

      test "compare sets" do
        Assert.equal LT $ compare (parseCirruEdn "#{}") (parseCirruEdn "#{} 1")
        Assert.equal LT $ compare (parseCirruEdn "#{}") (parseCirruEdn "#{} 1 2")
        Assert.equal LT $ compare (parseCirruEdn "#{}") (parseCirruEdn "{}")
        Assert.equal LT $ compare (parseCirruEdn "#{}") (parseCirruEdn "{} (:a 1)")
        Assert.equal EQ $ compare (parseCirruEdn "#{}") (parseCirruEdn "#{}")
        Assert.equal LT $ compare (parseCirruEdn "#{} 1") (parseCirruEdn "#{} 2")

      test "compare set and record" do
        Assert.equal LT $ compare (parseCirruEdn "{}") (parseCirruEdn "%{} name (a) (1)")
        Assert.equal GT $ compare (parseCirruEdn "%{} name (a) (1)") (parseCirruEdn "#{}")
        Assert.equal LT $ compare (parseCirruEdn "%{} name (a) (1)") (parseCirruEdn "%{} name (a) (2)")
        Assert.equal LT $ compare (parseCirruEdn "%{} a (a) (1)") (parseCirruEdn "%{} z (a) (2)")
        Assert.equal GT $ compare (parseCirruEdn "%{} z (a) (1)") (parseCirruEdn "%{} a (a) (2)")

      test "test quote" do
        Assert.equal LT $ compare (parseCirruEdn "quote $ def a") (parseCirruEdn "quote $ def b")
        Assert.equal EQ $ compare (parseCirruEdn "quote $ def a") (parseCirruEdn "quote $ def a")
        Assert.equal GT $ compare (parseCirruEdn "quote $ def b") (parseCirruEdn "quote $ def a")

        Assert.equal LT $ compare (parseCirruEdn "quote $ def a") (parseCirruEdn "#{}")
        Assert.equal LT $ compare (parseCirruEdn "do nil") (parseCirruEdn "quote $ def b")
