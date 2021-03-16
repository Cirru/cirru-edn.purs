module Cirru.Edn where

import Prelude

import Data.Eq
import Data.Map.Internal (Map)

import Cirru.Node (CirruNode(..))

data CirruEdn = CrEdnString String |
                CrEdnNumber Number |
                CrEdnKeyword String |
                CrEdnSymbol String |
                CrEdnBool Boolean |
                CrEdnList (Array CirruEdn) |
                CrEdnMap (Map CirruEdn CirruEdn) |
                CrEdnRecord String (Array String) (Array CirruEdn) |
                CrEdnQuote CirruNode |
                CrEdnNil

parseCirruEdn :: String -> CirruEdn
parseCirruEdn _ = CrEdnString "todo"

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
