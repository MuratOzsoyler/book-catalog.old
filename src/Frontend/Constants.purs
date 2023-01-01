module Frontend.Constants where

import Prelude
import Affjax as AJ
import Data.Array as Array
import Data.String as String

tokatHomeURL = "/tokat-home" :: AJ.URL
tokatFirstPageURL = "/tokat-first-page" :: AJ.URL
tokatNextPageURL = "/tokat-next-page" :: AJ.URL

bookOperationURL :: String -> String -> AJ.URL
bookOperationURL op isbn = String.joinWith "/" $ Array.filter (not <<< String.null) [ "/book", op, isbn ]

