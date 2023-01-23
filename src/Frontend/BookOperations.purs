module Frontend.BookOperations where

import Prelude

import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestHeader as AJRH
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web as AJ
import Common.BookProps (BookProps)
import Control.Monad.Except (ExceptT(..), except, throwError, withExceptT)
import Data.Argonaut.Decode (fromJsonString, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method as Method
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Frontend.Constants (bookOperationURL)

data BookOpResult
  = OK BookProps
  | NoContent
  | Created
  | NotFound
  | Conflict

getBook :: forall m. MonadAff m => String -> ExceptT String m BookOpResult
getBook isbn = do
  let
    headers = [ AJRH.Accept $ MediaType "application/json" ]
    url = bookOperationURL "" isbn
    responseFormat = RF.string
    req = AJ.defaultRequest { headers = headers, url = url, responseFormat = responseFormat }
  resp <- withExceptT AJ.printError $ ExceptT $ liftAff $ AJ.request req
  case resp.status of
    StatusCode 404 -> pure NotFound
    StatusCode 200 -> withExceptT printJsonDecodeError $ except $ OK <$> fromJsonString resp.body
    StatusCode stat -> throwError $ "error while getting book: " <> show stat <> " " <> resp.statusText <> ", " <> resp.body

updateBook :: forall m. MonadAff m => String -> BookProps -> ExceptT String m BookOpResult
updateBook isbn props = do
  let
    headers = [ AJRH.ContentType $ MediaType "application/json" ]
    url = bookOperationURL "update" isbn
    responseFormat = RF.string
    req = AJ.defaultRequest
      { headers = headers
      , url = url
      , method = Left Method.PUT
      , responseFormat = responseFormat
      , content = Just $ Json $ encodeJson props
      }
  resp <- withExceptT AJ.printError $ ExceptT $ liftAff $ AJ.request req
  case resp.status of
    StatusCode 404 -> pure NotFound
    StatusCode 204 -> pure NoContent
    StatusCode stat -> throwError $ "error while updating book: " <> show stat <> " " <> resp.statusText <> ", " <> resp.body

deleteBook :: forall m. MonadAff m => String -> ExceptT String m BookOpResult
deleteBook isbn = do
  resp <- withExceptT AJ.printError $ ExceptT $ liftAff $ AJ.delete RF.string $ bookOperationURL "delete" isbn
  case resp.status of
    StatusCode 404 -> pure NotFound
    StatusCode 204 -> pure NoContent
    StatusCode stat -> throwError $ "error while deleting book: " <> show stat <> " " <> resp.statusText <> ", " <> resp.body

addBook :: forall m. MonadAff m => String -> BookProps -> ExceptT String m BookOpResult
addBook isbn props = do
  let
    headers = [ AJRH.ContentType $ MediaType "application/json" ]
    url = bookOperationURL "add" isbn
    responseFormat = RF.string
    req = AJ.defaultRequest
      { headers = headers
      , url = url
      , method = Left Method.POST
      , responseFormat = responseFormat
      , content = Just $ Json $ encodeJson props
      }
  resp <- withExceptT AJ.printError $ ExceptT $ liftAff $ AJ.request req
  case resp.status of
    StatusCode 404 -> pure NotFound
    StatusCode 201 -> pure Created
    StatusCode stat -> throwError $ "error while updating book: " <> show stat <> " " <> resp.statusText <> ", " <> resp.body
