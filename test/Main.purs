module Test.Main where

import Prelude
import Cirru.Edn (CirruEdn(..), parseCirruEdn, writeCirruEdn)
import Cirru.Node (CirruNode(..))
import Data.Either (Either(..))
import Data.Map as DataMap
import Data.Map as Map
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

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
  case (parseCirruEdn "%{} P (name |Chen) (age 20)") of
    Right d -> log $ show d
    Left failure -> log $ "Failed at: " <> (show failure)
  runTest do
    suite "parse code" do
      test "parse quote" do
        Assert.equal (parseCirruEdn "quote $ demo") (Right $ CrEdnQuote $ CirruList [ CirruLeaf "demo" ])
      test "parse boolean" do
        Assert.equal (parseCirruEdn "do true") (Right $ CrEdnBool true)
      test "parse list" do
        Assert.equal (parseCirruEdn "[] true true true")
          (Right $ CrEdnList [ CrEdnBool true, CrEdnBool true, CrEdnBool true ])
      test "parse nested list" do
        Assert.equal (parseCirruEdn "[] true true $ [] true")
          (Right $ CrEdnList [ CrEdnBool true, CrEdnBool true, CrEdnList [ CrEdnBool true ] ])
      test "parse map" do
        Assert.equal (parseCirruEdn "{} (:a 1) (:b 2)")
          ( Right $ CrEdnMap
              $ DataMap.fromFoldable
                  [ Tuple (CrEdnKeyword "a") (CrEdnNumber 1.0)
                  , Tuple (CrEdnKeyword "b") (CrEdnNumber 2.0)
                  ]
          )
      test "parse record" do
        Assert.equal (Right $ CrEdnRecord "P" [ "name", "age" ] [ CrEdnString "Chen", CrEdnNumber 20.0 ])
          (parseCirruEdn "%{} P (name |Chen) (age 20)")
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
        Assert.equal LT $ compare (parseCirruEdn "{}") (parseCirruEdn "%{} name (a 1)")
        Assert.equal GT $ compare (parseCirruEdn "%{} name (a 1)") (parseCirruEdn "#{}")
        Assert.equal LT $ compare (parseCirruEdn "%{} name (a 1)") (parseCirruEdn "%{} name (a 2)")
        Assert.equal LT $ compare (parseCirruEdn "%{} a (a 1)") (parseCirruEdn "%{} z (a 2)")
        Assert.equal GT $ compare (parseCirruEdn "%{} z (a 1)") (parseCirruEdn "%{} a (a 2)")
      test "test quote" do
        Assert.equal LT $ compare (parseCirruEdn "quote $ def a") (parseCirruEdn "quote $ def b")
        Assert.equal EQ $ compare (parseCirruEdn "quote $ def a") (parseCirruEdn "quote $ def a")
        Assert.equal GT $ compare (parseCirruEdn "quote $ def b") (parseCirruEdn "quote $ def a")
        Assert.equal LT $ compare (parseCirruEdn "quote $ def a") (parseCirruEdn "#{}")
        Assert.equal LT $ compare (parseCirruEdn "do nil") (parseCirruEdn "quote $ def b")
  runTest do
    suite "generating" do
      test "compare format" do
        Assert.equal "do nil" (trim (writeCirruEdn CrEdnNil))
        Assert.equal "do true" (trim (writeCirruEdn (CrEdnBool true)))
        Assert.equal "do 1.0" (trim (writeCirruEdn (CrEdnNumber 1.0)))
        Assert.equal "[]" (trim (writeCirruEdn (CrEdnList [])))
        Assert.equal "[] :a" (trim (writeCirruEdn (CrEdnList [ CrEdnKeyword "a" ])))
        Assert.equal "{} (:a 1.0)\n  :b 2.0"
          ( trim
              ( writeCirruEdn
                  ( CrEdnMap
                      ( Map.fromFoldable
                          [ Tuple (CrEdnKeyword "a") (CrEdnNumber 1.0)
                          , Tuple (CrEdnKeyword "b") (CrEdnNumber 2.0)
                          ]
                      )
                  )
              )
          )
        case parseCirruEdn recordDemo1 of
          Right nodes -> Assert.equal recordDemo1 (writeCirruEdn nodes)
          Left errorNodes -> liftEffect (throw $ "Failed parsing: " <> show errorNodes)
        case parseCirruEdn dictDemo1 of
          Right nodes -> Assert.equal dictDemo1 (writeCirruEdn nodes)
          Left errorNodes -> liftEffect (throw $ "Failed parsing: " <> show errorNodes)

recordDemo1 :: String
recordDemo1 =
  """
%{} Demo (a 1.0)
  b 2.0
  c $ [] 1.0 2.0 3.0
"""

dictDemo1 :: String
dictDemo1 =
  """
{} (:a 1.0)
  :b $ [] 2.0 3.0 4.0
  :c $ {} (:d 4.0)
    :e true
    :f :g
    :h $ {} (|a 1.0)
      |b true
"""
