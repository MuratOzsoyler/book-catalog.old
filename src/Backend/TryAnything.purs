module TryAnything where

import Data.Either
import Prelude

import Data.Argonaut.Decode (JsonDecodeError, fromJsonString, printJsonDecodeError)
import Data.Argonaut.Encode (toJsonString)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console

trial :: Effect Unit
trial = do
  let a = Map.fromFoldable [Tuple "k1" ["v11", "v12"], Tuple "k2" ["v21"]]
      jsonStr = toJsonString a
      (a' :: Either JsonDecodeError (Map String (Array String))) = fromJsonString jsonStr

  Console.logShow a' 