module Cirru.Edn
  ( CirruEdn(..)
  , parseCirruEdn
  , writeCirruEdn
  ) where

import Prelude
import Cirru.Node (CirruNode(..), isCirruLeaf)
import Cirru.Parser (parseCirru)
import Cirru.Writer (writeCirru)
import Data.Array (head, length, zip, (!!), (:))
import Data.Array as DataArr
import Data.Either (Either(..))
import Data.Map as DataMap
import Data.Map as Map
import Data.Map.Internal (Map)
import Data.Maybe (Maybe(..), fromJust)
import Data.Number as DataNum
import Data.Set (Set, fromFoldable)
import Data.Set as DataSet
import Data.Set as Set
import Data.String.Common (joinWith)
import Data.String.NonEmpty.CodeUnits (charAt, drop)
import Data.String.NonEmpty.Internal (fromString, toString)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

-- | only uused for displaying, internall it's using Tuple
data CrEdnKv
  = CrEdnKv CirruEdn CirruEdn

instance showCrEdnKv :: Show CrEdnKv where
  show (CrEdnKv k v) = "(" <> (show k) <> " " <> (show v) <> ")"

instance eqCrEdnKv :: Eq CrEdnKv where
  eq (CrEdnKv k1 v1) (CrEdnKv k2 v2) = k1 == k2 && v1 == v2

-- | short handle accessing Array
arrayGet :: forall a. Array a -> Int -> a
arrayGet xs n = unsafePartial $ fromJust $ xs !! n

-- | data structure for Cirru EDN.Boolean
-- | notice that Map and Set are not fully realized
data CirruEdn
  = CrEdnNil
  | CrEdnBool Boolean
  | CrEdnNumber Number
  | CrEdnSymbol String
  | CrEdnKeyword String
  | CrEdnString String
  | CrEdnQuote CirruNode
  | CrEdnList (Array CirruEdn)
  | CrEdnSet (Set CirruEdn)
  | CrEdnMap (Map CirruEdn CirruEdn)
  | CrEdnRecord String (Array String) (Array CirruEdn)

-- | if parsing failed, original Cirru Nodes are returned
type CrEdnParsed
  = Either CirruNode CirruEdn

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
        if (matchFloat s) then case (DataNum.fromString s) of
          Just n -> pure $ CrEdnNumber n
          Nothing -> Left (CirruLeaf s)
        else
          Left (CirruLeaf s)
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
      "%{}" ->
        if (length xs) == 4 then
          extractRecord (arrayGet xs 1) (arrayGet xs 2) (arrayGet xs 3)
        else
          Left (CirruList xs)
      _ -> Left (CirruList xs)

-- | turn CirruEdn into CirruNode
assembleCirruNode :: CirruEdn -> CirruNode
assembleCirruNode edn = case edn of
  CrEdnNil -> CirruLeaf "nil"
  CrEdnBool t -> CirruLeaf (show t)
  CrEdnNumber n -> CirruLeaf (show n)
  CrEdnSymbol s -> CirruLeaf ("'" <> s)
  CrEdnKeyword s -> CirruLeaf (":" <> s)
  CrEdnString s -> CirruLeaf ("|" <> s)
  CrEdnQuote xs -> CirruList [ CirruLeaf "quote", xs ]
  CrEdnList xs -> CirruList ((CirruLeaf "[]") : (map assembleCirruNode xs))
  CrEdnSet xs -> CirruList ((CirruLeaf "#{}") : (map assembleCirruNode (Set.toUnfoldable xs)))
  CrEdnMap m -> CirruList ((CirruLeaf "{}") : pairs)
    where
    pairs =
      map
        ( \(Tuple k v) ->
            CirruList [ assembleCirruNode k, assembleCirruNode v ]
        )
        (Map.toUnfoldable m)
  CrEdnRecord name fields values -> CirruList ((CirruLeaf "%{}") : (CirruLeaf name) : pairs)
    where
    pairs =
      map
        ( \(Tuple k v) ->
            CirruList [ assembleCirruNode (CrEdnString k), assembleCirruNode v ]
        )
        (zip fields values)

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

-- TODO syntax is wrong, need to rewrite
extractRecord :: CirruNode -> CirruNode -> CirruNode -> CrEdnParsed
extractRecord name fields values =
  let
    lengthMatched = case fields, values of
      (CirruList xs), (CirruList ys) -> (length xs) == (length ys)
      _, _ -> false

    fitRecord = (isCirruLeaf name) && lengthMatched && allLeaves fields
  in
    if fitRecord then do
      nameInString <- getLeafStr name
      fieldsString <- getLeavesStr fields
      valueItems <- case values of
        CirruList xs -> traverse extractCirruEdn xs
        CirruLeaf _ -> Left values
      Right $ CrEdnRecord nameInString fieldsString valueItems
    else
      Left $ CirruList [ name, fields, values ]

extractKeyValuePair :: CirruNode -> Either CirruNode (Tuple CirruEdn CirruEdn)
extractKeyValuePair (CirruLeaf s) = Left (CirruLeaf s)

extractKeyValuePair (CirruList xs) =
  if (length xs) == 2 then do
    k <- extractCirruEdn $ arrayGet xs 0
    v <- extractCirruEdn $ arrayGet xs 1
    Right $ Tuple k v
  else
    Left (CirruList xs)

-- | extract Map
extractMap :: (Array CirruNode) -> CrEdnParsed
extractMap xs = do
  ys <- traverse extractKeyValuePair xs
  Right $ CrEdnMap (DataMap.fromFoldable ys)

-- | extract Cirru EDN list from Cirru Nodes
extractList :: (Array CirruNode) -> CrEdnParsed
extractList xs = do
  ys <- traverse extractCirruEdn xs
  Right $ CrEdnList ys

-- | extract Set
extractSet :: (Array CirruNode) -> CrEdnParsed
extractSet xs = do
  ys <- traverse extractCirruEdn xs
  Right $ CrEdnSet (fromFoldable ys)

-- | parse String content into Cirru EDN structure,
-- | returns original Cirru Nodes if pasing failed.
-- | might be hard to figure out reason sometimes since failure not detailed
parseCirruEdn :: String -> CrEdnParsed
parseCirruEdn s = case (parseCirru s) of
  CirruLeaf leaf -> Left (CirruLeaf leaf)
  CirruList xs ->
    if xs == [] then
      Left (CirruList [])
    else if (length xs) /= 1 then
      Left (CirruList xs)
    else case (head xs) of
      Just ys -> case ys of
        CirruLeaf _ -> Left (CirruList xs)
        CirruList _ -> extractCirruEdn ys
      Nothing -> Left (CirruList xs)

-- | generate Cirru code from Cirru EDN data
writeCirruEdn :: CirruEdn -> String
writeCirruEdn edn = case assembleCirruNode edn of
  CirruLeaf x -> writeCirru (CirruList [ (CirruList [ CirruLeaf "do", CirruLeaf x ]) ]) { useInline: false }
  CirruList xs -> writeCirru (CirruList [ (CirruList xs) ]) { useInline: false }

instance cirruEdnEq :: Eq CirruEdn where
  eq CrEdnNil CrEdnNil = true
  eq (CrEdnString x) (CrEdnString y) = x == y
  eq (CrEdnKeyword x) (CrEdnKeyword y) = x == y
  eq (CrEdnSymbol x) (CrEdnSymbol y) = x == y
  eq (CrEdnNumber x) (CrEdnNumber y) = x == y
  eq (CrEdnBool x) (CrEdnBool y) = x == y
  eq (CrEdnQuote x) (CrEdnQuote y) = x == y
  eq (CrEdnList xs) (CrEdnList ys) = xs == ys
  eq (CrEdnSet xs) (CrEdnSet ys) = xs == ys
  eq (CrEdnMap xs) (CrEdnMap ys) = xs == ys
  eq (CrEdnRecord xName xFields xs) (CrEdnRecord yName yFields ys) = xName == yName && xFields == yFields && xs == ys
  eq _ _ = false

setToArray :: forall k. Set k -> Array k
setToArray = DataSet.toUnfoldable

tupleToPair :: (Tuple CirruEdn CirruEdn) -> CrEdnKv
tupleToPair (Tuple x y) = CrEdnKv x y

mapToArray :: forall k v. Map k v -> Array (Tuple k v)
mapToArray = DataMap.toUnfoldable

instance showCirruEdn :: Show CirruEdn where
  show CrEdnNil = "nil"
  show (CrEdnBool x) = show x
  show (CrEdnNumber x) = show x
  show (CrEdnSymbol x) = "'" <> x
  show (CrEdnKeyword x) = ":" <> x
  show (CrEdnString x) = "|" <> x
  show (CrEdnQuote x) = "(quote " <> (show x) <> ")"
  show (CrEdnList xs) = "([] " <> (joinWith " " (map show xs)) <> ")"
  show (CrEdnSet xs) = "(#{} " <> (joinWith " " (map show (setToArray xs))) <> ")"
  show (CrEdnMap xs) = "({} " <> (joinWith " " (map show (map tupleToPair (mapToArray xs)))) <> ")"
  show (CrEdnRecord name fields values) =
    "(%{} " <> name
      <> " ("
      <> (joinWith " " fields)
      <> ")"
      <> " ("
      <> (joinWith " " (map show values))
      <> ")"

instance ordCrEdnKv :: Ord CrEdnKv where
  compare (CrEdnKv k1 v1) (CrEdnKv k2 v2) = case compare k1 k2 of
    LT -> LT
    GT -> GT
    EQ -> compare k1 k2

instance ordCirruEdn :: Ord CirruEdn where
  compare CrEdnNil CrEdnNil = EQ
  compare CrEdnNil _ = LT
  compare _ CrEdnNil = GT
  compare (CrEdnBool false) (CrEdnBool true) = LT
  compare (CrEdnBool true) (CrEdnBool false) = GT
  compare (CrEdnBool _) (CrEdnBool _) = EQ
  compare (CrEdnBool _) _ = LT
  compare _ (CrEdnBool _) = GT
  compare (CrEdnNumber x) (CrEdnNumber y) = compare x y
  compare (CrEdnNumber x) _ = LT
  compare _ (CrEdnNumber x) = GT
  compare (CrEdnSymbol x) (CrEdnSymbol y) = compare x y
  compare (CrEdnSymbol x) _ = LT
  compare _ (CrEdnSymbol x) = GT
  compare (CrEdnKeyword x) (CrEdnKeyword y) = compare x y
  compare (CrEdnKeyword x) _ = LT
  compare _ (CrEdnKeyword x) = GT
  compare (CrEdnString x) (CrEdnString y) = compare x y
  compare (CrEdnString x) _ = LT
  compare _ (CrEdnString x) = GT
  compare (CrEdnQuote x) (CrEdnQuote y) = compare x y
  compare (CrEdnQuote x) _ = LT
  compare _ (CrEdnQuote x) = GT
  compare (CrEdnList xs) (CrEdnList ys) = case (compare (length xs) (length ys)) of
    LT -> LT
    GT -> GT
    EQ -> compare xs ys
  compare (CrEdnList xs) _ = LT
  compare _ (CrEdnList xs) = GT
  compare (CrEdnSet xs) (CrEdnSet ys) = case (compare (DataSet.size xs) (DataSet.size ys)) of
    LT -> LT
    GT -> GT
    EQ -> compare xs ys
  compare (CrEdnSet xs) _ = LT
  compare _ (CrEdnSet xs) = GT
  compare (CrEdnMap xs) (CrEdnMap ys) = case (compare (DataMap.size xs) (DataMap.size ys)) of
    LT -> LT
    GT -> GT
    EQ -> compare xs ys
  compare (CrEdnMap xs) _ = LT
  compare _ (CrEdnMap xs) = GT
  compare (CrEdnRecord name1 fields1 values1) (CrEdnRecord name2 fields2 values2) = case compare name1 name2 of
    LT -> LT
    GT -> GT
    EQ -> case compare fields1 fields2 of
      LT -> LT
      GT -> GT
      EQ -> compare values1 values2
