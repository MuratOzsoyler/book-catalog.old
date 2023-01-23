module Common.BookProps where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.String (Pattern(..), joinWith)
import Data.String as String
import Data.Tuple.Nested (type (/\))
import Effect (Effect)

type ISBN = String

type BookPropName = String

type BookPropValue = String

type BookPropValues = Array String

type BookProps = Map BookPropName BookPropValues

type BookProp = BookPropName /\ BookPropValues

type SetBookProp = BookProp -> Effect Unit

bookPropSeparator = "¦" :: String

propsToBook ∷ String → Map String (Array String) → Map String String
propsToBook isbn props = Map.insert "ISBN" isbn $ map (joinWith bookPropSeparator) props

bookToProps ∷ Map String String -> Map String (Array String)
bookToProps book = map (String.split $ Pattern bookPropSeparator) $ Map.delete "ISBN" book

