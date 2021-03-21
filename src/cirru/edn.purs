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
import Partial.Unsafe (unsafePartial)

-- | Used inside Maps
data CrEdnKv = CrEdnKv CirruEdn CirruEdn

instance showCrEdnKv :: Show CrEdnKv where
  show (CrEdnKv k v) = "(" <> (show k) <> " " <> (show v) <> ")"

instance eqCrEdnKv :: Eq CrEdnKv where
  eq (CrEdnKv k1 v1) (CrEdnKv k2 v2) = k1 == k2 && v1 == v2

-- | short handle accessing Array
arrayGet :: forall a. Array a -> Int -> a
arrayGet xs n = unsafePartial $ fromJust $ xs !! n

-- | data structure for Cirru EDN.Boolean
-- | notice that Map and Set are not fully realized
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

-- | if parsing failed, original Cirru Nodes are returned
type CrEdnParsed = Either CirruNode CirruEdn

-- | tests if thats a float
matchFloat :: String -> Boolean
matchFloat s = case (regex "^-?(\\d+)(\\.\\d*)?$" noFlags) of
  Right pattern -> test pattern s
  Left failure -> false

-- | extra from Cirru Nodes
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
      "%{}" -> if (length xs) == 4
        then extractRecord (arrayGet xs 1) (arrayGet xs 2) (arrayGet xs 3)
        else Left (CirruList xs)
      _ -> Left (CirruList xs)

-- | returns false if it's a leaf,
-- | returns false if there's array inside array
allLeaves :: CirruNode -> Boolean
allLeaves ys = case ys of
  CirruLeaf _ -> false
  CirruList xs -> case xs !! 0 of
    Just x0 -> case x0 of
      CirruList _ -> false
      CirruLeaf _ -> allLeaves $ CirruList (DataArr.drop 1 xs)
    Nothing -> true

isLeaf :: CirruNode -> Boolean
isLeaf x = case x of
  CirruLeaf _ -> true
  CirruList _ -> false

getLeafStr :: CirruNode -> Either CirruNode String
getLeafStr (CirruList xs) = Left (CirruList xs)
getLeafStr (CirruLeaf s) = Right s

getLeavesStr :: CirruNode -> Either CirruNode (Array String)
getLeavesStr ys = case ys of
  CirruLeaf _ -> Left ys
  CirruList xs -> case xs !! 0 of
    Nothing -> Right []
    Just x0 -> case x0 of
      CirruLeaf s -> do
        follows <- getLeavesStr $ CirruList (DataArr.drop 1 xs)
        Right $ DataArr.cons s follows
      CirruList zs -> Left (CirruList zs)

extractRecord :: CirruNode -> CirruNode -> CirruNode -> CrEdnParsed
extractRecord name fields values = let
    lengthMatched = case fields, values of
      (CirruList xs), (CirruList ys) -> (length xs) == (length ys)
      _, _ -> false
    fitRecord = (isLeaf name) && lengthMatched && allLeaves fields
  in if fitRecord
    then do
      nameInString <- getLeafStr name
      fieldsString <- getLeavesStr fields
      valueItems <- case values of
        CirruList xs -> traverse extractCirruEdn xs
        CirruLeaf _ -> Left values
      Right $ CrEdnRecord nameInString fieldsString valueItems
    else Left $ CirruList [name, fields, values]

extractKeyValuePair :: CirruNode -> Either CirruNode CrEdnKv
extractKeyValuePair (CirruLeaf s) = Left (CirruLeaf s)
extractKeyValuePair (CirruList xs) = if (length xs) == 2
  then do
    k <- extractCirruEdn $ arrayGet xs 0
    v <- extractCirruEdn $ arrayGet xs 1
    Right $ CrEdnKv k v
  else Left (CirruList xs)

-- | TODO, inherit Ord for Map
extractMap :: (Array CirruNode) -> CrEdnParsed
extractMap xs = do
  ys <- traverse extractKeyValuePair xs
  Right $ CrEdnMap ys

-- | extract Cirru EDN list from Cirru Nodes
extractList :: (Array CirruNode) -> CrEdnParsed
extractList xs = do
  ys <- traverse extractCirruEdn xs
  Right $ CrEdnList ys

-- | TODO, inherit Ord for Set
extractSet :: (Array CirruNode) -> CrEdnParsed
extractSet xs = do
  ys <- traverse extractCirruEdn xs
  Right $ CrEdnSet ys

-- | parse String content into Cirru EDN structure,
-- | returns original Cirru Nodes if pasing failed.
-- | might be hard to figure out reason sometimes since failure not detailed
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
  eq (CrEdnNumber x) (CrEdnNumber y) = x == y
  eq (CrEdnBool x) (CrEdnBool y) = x == y
  eq (CrEdnQuote x) (CrEdnQuote y) = x == y
  eq (CrEdnList xs) (CrEdnList ys) = xs == ys
  eq (CrEdnSet xs) (CrEdnSet ys) = xs == ys -- TODO need to handle Ord
  eq (CrEdnMap xs) (CrEdnMap ys) = xs == ys
  eq (CrEdnRecord xName xFields xs) (CrEdnRecord yName yFields ys) =
    xName == yName && xFields == yFields && xs == ys
  eq _ _ = false

instance showCirruEdn :: Show CirruEdn where
  show CrEdnNil = "nil"
  show (CrEdnBool x) = show x
  show (CrEdnKeyword x) = ":" <> x
  show (CrEdnSymbol x) = "'" <> x
  show (CrEdnString x) = "|" <> x
  show (CrEdnNumber x) = show x
  show (CrEdnQuote x) = "(quote " <> (show x) <> ")"
  show (CrEdnList xs) = "([] " <> (joinWith " " (map show xs)) <> ")"
  show (CrEdnSet xs) = "(#{} " <> (joinWith " " (map show xs)) <> ")"
  show (CrEdnMap xs) = "({} " <> (joinWith " " (map show xs)) <> ")"
  show (CrEdnRecord name fields values) = "(%{} " <> name <>
    " (" <> (joinWith " " fields) <> ")" <>
    " (" <> (joinWith " " (map show values)) <> ")"
