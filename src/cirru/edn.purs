module Cirru.Edn where

import Data.Either
import Data.Eq
import Prelude

import Cirru.Node (CirruNode(..))
import Cirru.Parser (parseCirru)
import Data.Array (head, length, (!!))
import Data.Array as DataArr
import Data.Either (Either(..))
import Data.Map.Internal (Map)
import Data.Maybe (Maybe(..), fromJust)
import Data.Number as DataNum
import Data.Set (Set, fromFoldable, toUnfoldable)
import Data.String.Common (joinWith)
import Data.String.NonEmpty.CodeUnits (charAt, drop)
import Data.String.NonEmpty.Internal (fromString, unsafeFromString, toString)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw, error)
import Partial.Unsafe (unsafePartial)
import Debug (spy)

type CrEdnKv = { k :: CirruEdn, v :: CirruEdn }

-- TODO
-- newtype CrEdnKv = CrEdnKv { k :: CirruEdn, v :: CirruEdn }

-- instance showCrEdnKv :: Show CrEdnKv where
--   show x = (show x.k) <> (show x.v)

data CirruEdn = CrEdnString String |
                CrEdnNumber Number |
                CrEdnKeyword String |
                CrEdnSymbol String |
                CrEdnBool Boolean |
                CrEdnList (Array CirruEdn) |
                -- for EDN, use Array for convenience
                -- need extra handling when hydrating into data
                CrEdnSet (Array CirruEdn) |
                CrEdnMap (Array CrEdnKv) |
                CrEdnRecord String (Array String) (Array CirruEdn) |
                CrEdnQuote CirruNode |
                CrEdnNil

type CrEdnParsed = Either CirruNode CirruEdn

matchFloat :: String -> Boolean
matchFloat s = case (regex "^-?(\\d+)(\\.\\d*)?$" noFlags) of
  Right pattern -> test pattern s
  Left failure -> false

extractCirruEdn :: CirruNode -> CrEdnParsed
extractCirruEdn (CirruLeaf s) = case (fromString s) of
  Just ns -> case (charAt 0 ns) of
    Just '\'' -> case (drop 1 ns) of
      Just result -> pure $ CrEdnSymbol $ toString result
      Nothing -> Left (CirruLeaf s)
    Just ':' -> case (drop 1 ns) of
      Just result -> pure $ CrEdnKeyword $ toString result
      Nothing -> Left (CirruLeaf s)
    Just '"' -> case (drop 1 ns) of
      Just result -> pure $ CrEdnString $ toString result
      Nothing -> Left (CirruLeaf s)
    Just '|' -> case (drop 1 ns) of
      Just result -> pure $ CrEdnString $ toString result
      Nothing -> Left (CirruLeaf s)
    Just _ -> case s of
      "true" -> pure $ CrEdnBool true
      "false" -> pure $ CrEdnBool false
      "nil" -> pure $ CrEdnNil
      _ ->
        if (matchFloat s)
          then case (DataNum.fromString s) of
            Just n -> pure $ CrEdnNumber n
            Nothing -> Left (CirruLeaf s)
        else Left (CirruLeaf s)
    Nothing -> Left (CirruLeaf s)
  Nothing -> Left (CirruLeaf s)

extractCirruEdn (CirruList xs) = case (xs !! 0) of
  Nothing -> Left (CirruList xs)
  Just x0 -> case x0 of
    CirruList _ -> Left (CirruList xs)
    CirruLeaf x -> case x of
      "quote" -> case (xs !! 1) of
        Nothing -> Left (CirruList xs)
        Just content -> pure $ CrEdnQuote content
      "do" -> case (xs !! 1) of
        Nothing -> Left (CirruList xs)
        Just content -> extractCirruEdn content
      "[]" -> extractList (DataArr.drop 1 xs)
      "{}" -> extractMap (DataArr.drop 1 xs)
      "#{}" -> extractSet (DataArr.drop 1 xs)
      "%{}" -> pure $ CrEdnNil
      _ -> Left (CirruList xs)

extractKeyValuePair :: CirruNode -> Either CirruNode CrEdnKv
extractKeyValuePair (CirruLeaf s) = Left (CirruLeaf s)
extractKeyValuePair (CirruList xs) = if (length xs) == 2
  then do
    k <- extractCirruEdn $ unsafePartial $ fromJust $ xs !! 0
    v <- extractCirruEdn $ unsafePartial $ fromJust $ xs !! 1
    Right {k: k, v: v}
  else Left (CirruList xs)

extractMap :: (Array CirruNode) -> CrEdnParsed
extractMap xs = do
  ys <- traverse extractKeyValuePair xs
  Right $ CrEdnMap ys

extractList :: (Array CirruNode) -> CrEdnParsed
extractList xs = do
  ys <- traverse extractCirruEdn xs
  Right $ CrEdnList ys

extractSet :: (Array CirruNode) -> CrEdnParsed
extractSet xs = do
  ys <- traverse extractCirruEdn xs
  Right $ CrEdnSet ys

parseCirruEdn :: String -> CrEdnParsed
parseCirruEdn s = case (parseCirru s) of
  CirruLeaf leaf -> Left (CirruLeaf leaf)
  CirruList xs -> if xs == []
    then Left (CirruList [])
    else if (length xs) /= 1
      then Left (CirruList xs)
      else case (head xs) of
        Just ys -> case ys of
          CirruLeaf _ -> Left (CirruList xs)
          CirruList _ -> extractCirruEdn ys
        Nothing -> Left (CirruList xs)

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
  show (CrEdnKeyword x) = ":" <> x
  show (CrEdnSymbol x) = "'" <> x
  show (CrEdnNumber x) = show x
  show (CrEdnQuote x) = "(quote " <> (show x) <> ")"
  show (CrEdnList xs) = "([] " <> (joinWith " " (map show xs)) <> ")"
  show (CrEdnSet xs) = "(#{} " <> (joinWith " " (map show xs)) <> ")"
  show (CrEdnMap xs) = "({} " <> (joinWith ", " (map show xs)) <> ")"
  show _  = "TODO"
