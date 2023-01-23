module Backend.BookOperations where

import Prelude

import Common.BookProps (BookProps, bookToProps, propsToBook)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), catchError, throwError, withExceptT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List, (!!))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.String (Pattern(..), Replacement(..), joinWith)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)
import Parsing (parseErrorMessage, runParser)
import Parsing.CSV (makeParsers)
import Parsing.CSV as CSV
import Partial.Unsafe (unsafePartial)

-- import Common.BookProps ()

csvFilePath = "./books.csv" :: String

readCsvFile :: forall m. MonadAff m => FilePath -> ExceptT String m String
readCsvFile filepath = withExceptT show $ ExceptT $ liftAff $ try $ readTextFile UTF8 filepath

writeCsvFile :: forall m. MonadAff m => FilePath -> String -> ExceptT String m Unit
writeCsvFile filepath content = liftAff $ writeTextFile UTF8 filepath content

makeCsvParsers :: CSV.Parsers String
makeCsvParsers = makeParsers '"' ";" "\n"

printCsv :: List (Map String String) -> String
printCsv books =
  let
    books' = map Map.toUnfoldable $ List.toUnfoldable books
  in
    if Array.null books' then ""
    else
      let
        header = map (\(Tuple k _) -> wrapWithQuotes k) $ unsafePartial $ fromJust $ Array.head books'
        hdrstr = String.joinWith ";" header
      in
        String.joinWith "\n" $ hdrstr Array.: map (joinWith ";" <<< map \(Tuple _ v) -> wrapWithQuotes v) books'
  where
  encodeStr = String.replaceAll (Pattern "\"") (Replacement "\"\"")
  wrapWithQuotes str = "\"" <> encodeStr str <> "\""

withCsvFile
  :: forall m
   . MonadAff m
  => FilePath
  -> (List (Map String String) -> ExceptT String m (List (Map String String)))
  -> ExceptT String m Unit
withCsvFile filePath f = do
  books' <- withCsvFile' filePath f
  let csvstr' = printCsv books'
  writeCsvFile filePath csvstr'

withCsvFile'
  :: forall m a
   . MonadAff m
  => FilePath
  -> (List (Map String String) -> ExceptT String m a)
  -> ExceptT String m a
withCsvFile' filePath f = do
  csvstr <- readCsvFile filePath
  let parsers = makeCsvParsers
  case runParser csvstr parsers.fileHeaded of
    Left err -> throwError $ parseErrorMessage err
    Right books -> f books

withNewCsvFile
  :: forall m
   . MonadAff m
  => FilePath
  -> (List (Map String String) -> ExceptT String m (List (Map String String)))
  -> ExceptT String m Unit
withNewCsvFile filePath f = do
  let newBooks = mempty
  newBooks' <- f newBooks
  let csvstr' = printCsv newBooks'
  writeCsvFile filePath csvstr'

findBookIndex ∷ String -> List (Map String String) -> Maybe Int
findBookIndex isbn books = List.findIndex
  (\m -> fromMaybe false (pure <<< (_ == isbn) =<< Map.lookup "ISBN" m))
  books

addBook ∷ forall m. MonadAff m => String -> BookProps -> ExceptT String m Unit
addBook isbn props =
  catchError
    (withCsvFile csvFilePath go)
    \err -> do
      let errStr = {- show -}  err
      liftEffect $ Console.log $ "Caught error: " <> show err
      if String.contains (Pattern "ENOENT: no such file or directory") errStr then do
        liftEffect $ Console.log $ "File \"" <> csvFilePath <> "\" could not be found. An empty file will be created."
        withNewCsvFile csvFilePath go
      else
        throwError errStr
  where
  go books = do
    let found = List.any (maybe false (_ == isbn) <<< Map.lookup "ISBN") books
    if found then
      throwError "ISBN has been found in data file. Try deleting it first."
    else do
      let
        row = propsToBook isbn props
        books' = List.snoc books row
      pure books'

deleteBook :: forall m. MonadAff m => String -> ExceptT String m Unit
deleteBook isbn = withCsvFile csvFilePath \books -> do
  let
    mbBooks' = do
      idx <- findBookIndex isbn books
      List.deleteAt idx books
  case mbBooks' of
    Nothing -> throwError $ "The book with ISBN " <> isbn <> " could not be found"
    Just books' -> pure books'

updateBook :: forall m. MonadAff m => String -> BookProps -> ExceptT String m Unit
updateBook isbn props = withCsvFile csvFilePath \books -> do
  let
    mbBooks' = do
      idx <- findBookIndex isbn books
      List.updateAt idx (propsToBook isbn props) books
  case mbBooks' of
    Nothing -> throwError $ "The book with ISBN " <> isbn <> " could not be found"
    Just books' -> pure books'

getBook :: forall m. MonadAff m => String -> ExceptT String m BookProps
getBook isbn = withCsvFile' csvFilePath \books -> do
  let
    mbBook = do
      idx <- findBookIndex isbn books
      books !! idx
  case mbBook of
    Nothing -> throwError $ "The book with ISBN " <> isbn <> " could not be found"
    Just book -> pure $ bookToProps book