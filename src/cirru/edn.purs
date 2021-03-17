module Cirru.Edn where

import Prelude

import Data.Eq
import Data.Either
import Data.Maybe (Maybe(..))
import Data.Map.Internal (Map)
import Data.Number as DataNum
import Data.Array (head, length, (!!))
import Data.Set (Set)

import Effect.Class.Console (log)

import Data.String.NonEmpty.CodeUnits (charAt, drop)
import Data.String.NonEmpty.Internal (fromString, toString)

import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)

import Effect (Effect)
import Effect.Exception (throw, error)

import Cirru.Node (CirruNode(..))
import Cirru.Parser (parseCirru)

data CirruEdn = CrEdnString String |
                CrEdnNumber Number |
                CrEdnKeyword String |
                CrEdnSymbol String |
                CrEdnBool Boolean |
                CrEdnList (Array CirruEdn) |
                CrEdnSet (Set CirruEdn) |
                CrEdnMap (Map CirruEdn CirruEdn) |
                CrEdnRecord String (Array String) (Array CirruEdn) |
                CrEdnQuote CirruNode |
                CrEdnNil

matchFloat :: String -> Effect Boolean
matchFloat s = case (regex "^-?(\\d+)(\\.\\d*)?$" noFlags) of
  Right pattern -> pure $ test pattern s
  Left failure -> throw "Invalid regex expression"

extractCirruEdn :: CirruNode -> Effect CirruEdn
extractCirruEdn (CirruLeaf s) = case (fromString s) of
  Just ns -> case (charAt 0 ns) of
    Just '\'' -> case (drop 1 ns) of
      Just result -> pure $ CrEdnSymbol $ toString result
      Nothing -> throw "failed drop"
    Just ':' -> case (drop 1 ns) of
      Just result -> pure $ CrEdnKeyword $ toString result
      Nothing -> throw "failed drop"
    Just '"' -> case (drop 1 ns) of
      Just result -> pure $ CrEdnString $ toString result
      Nothing -> throw "failed drop"
    Just '|' -> case (drop 1 ns) of
      Just result -> pure $ CrEdnString $ toString result
      Nothing -> throw "failed drop"
    Just _ -> case s of
      "true" -> pure $ CrEdnBool true
      "false" -> pure $ CrEdnBool false
      "nil" -> pure $ CrEdnNil
      _ -> do
        isNumber <- matchFloat s
        if isNumber
          then case (DataNum.fromString s) of
            Just n -> pure $ CrEdnNumber n
            Nothing -> throw "Unknown token, failed to parse number"
        else throw "Unknown token, failed to parse"
    Nothing -> throw "Invalid char from NonEmpty string"
  Nothing -> throw "Empty string is invalid"

extractCirruEdn (CirruList xs) = case (xs !! 0) of
  Nothing -> throw "lack of head in expression"
  Just x0 -> case x0 of
    CirruList _ -> throw "Expected operator at head"
    CirruLeaf x -> case x of
      "quote" -> case (xs !! 1) of
        Nothing -> throw "missing quote content"
        Just content -> pure $ CrEdnQuote content
      "do" -> case (xs !! 1) of
        Nothing -> throw "missing do content"
        Just content -> extractCirruEdn content
      "[]" -> pure $ CrEdnNil
      "{}" -> pure $ CrEdnNil
      "#{}" -> pure $ CrEdnNil
      "%{}" -> pure $ CrEdnNil
      _ -> throw "Unknown syntax for data"

parseCirruEdn :: String -> Effect CirruEdn
parseCirruEdn s = case (parseCirru s) of
  CirruLeaf _ -> throw "Never got top level token"
  CirruList xs -> if xs == []
    then do
      log $ show xs
      throw "Cannot be empty list"
    else if (length xs) /= 1
      then throw "Supposed to be only one item"
      else case (head xs) of
        Just ys -> case ys of
          CirruLeaf _ -> throw "Expected list for EDN at first level"
          CirruList xs -> extractCirruEdn ys
        Nothing -> throw "cannot be empty"

instance cirruEdnEq :: Eq CirruEdn where
  eq CrEdnNil CrEdnNil = true
  eq (CrEdnString x) (CrEdnString y) = x == y
  eq (CrEdnKeyword x) (CrEdnKeyword y) = x == y
  eq (CrEdnSymbol x) (CrEdnSymbol y) = x == y
  eq (CrEdnBool x) (CrEdnBool y) = x == y
  eq (CrEdnQuote x) (CrEdnQuote y) = x == y
  eq (CrEdnList xs) (CrEdnList ys) = xs == ys
  eq (CrEdnMap xs) (CrEdnMap ys) = xs == ys
  eq (CrEdnRecord xName xFields xs) (CrEdnRecord yName yFields ys) =
    xName == yName && xFields == yFields && xs == ys
  eq _ _ = false

instance showCirruEdn :: Show CirruEdn where
  show CrEdnNil = "nil"
  show (CrEdnBool x) = show x
  show (CrEdnQuote x) = "(quote " <> (show x) <> ")"
  show _  = "TODO"
