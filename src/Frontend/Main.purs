module Frontend.Main where

import Prelude

import Control.Plus (class Plus)
import Deku.Attribute (Attribute)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Frontend.UI (bookPropsInputPage)

main :: Effect Unit
main = runInBody bookPropsInputPage