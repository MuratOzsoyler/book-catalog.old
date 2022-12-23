module Backend.Main where

import Prelude hiding ((/))

import Affjax.Node (URL, get, printError)
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Data.Array (elem)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Profunctor (dimap)
import Data.String (Pattern(..), joinWith, split)
import Debug (traceM)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import HTTPurple (Request, ResponseM, ServerM, as, header, int, internalServerError, mkRoute, noArgs, notFound, ok, ok', response, rest, serve, string, (/), (?))
import HTTPurple.Headers (ResponseHeaders)
import HTTPurple.Status (misdirectedRequest)
import Node.FS.Aff (readFile)
import Node.Path (FilePath, concat, extname)
import Node.Process (cwd)
import Node.URL (Query, format, parse, toQueryString)
import Partial.Unsafe (unsafeCrashWith)
import Routing.Duplex (RouteDuplex')
import Unsafe.Coerce (unsafeCoerce)

data Route
  = Home
  | Asset Asset
  | TokatHome
  | TokatFirstPage
      { _f :: String -- _f
      , the_page :: String -- the_page
      , cwid :: Int -- cwid
      , keyword :: String --keyword
      , tokat_search_field :: Int -- tokat_search_field
      , order :: Int -- order
      , command :: String -- command
      }
  | TokatNextPage
      { cwid :: Int -- cwid
      , keyword :: String --keyword
      , tokat_search_field :: Int -- tokat_search_field
      , order :: Int -- order
      , ts :: Int -- ts
      , page :: Int -- page
      }

derive instance Generic Route _

data Asset = PrimaryAsset String | DependencyAsset String String

derive instance Generic Asset _

assetToString :: Asset -> String
assetToString = case _ of
  PrimaryAsset name -> assetsPath <> "/" <> name
  DependencyAsset pth name -> concat [ dependencyPath, pth, name ]

assetFromString :: String -> Either String Asset
assetFromString s = case split (Pattern "/") s of
  [ pth, name ]
    | pth == assetsPath -> Right $ PrimaryAsset name
  [ depPth, pth, name ]
    | depPth == dependencyPath -> Right $ DependencyAsset pth name
  _ -> Left $ "Not an asset: \"" <> s <> "\""

asset :: RouteDuplex' String -> RouteDuplex' Asset
asset = as assetToString assetFromString

route :: RouteDuplex' Route
route = mkRoute
  { "Home": noArgs
  , "Asset": asset $ dimap (split $ Pattern "/") (joinWith "/") rest
  , "TokatHome": "tokat-home" / noArgs
  , "TokatFirstPage": "tokat-first-page" ?
      { _f: string
      , the_page: string
      , cwid: int
      , keyword: string
      , tokat_search_field: int
      , order: int
      , command: string
      }
  , "TokatNextPage": "tokat-next-page" ?
      { cwid: int
      , keyword: string
      , tokat_search_field: int
      , order: int
      , ts: int
      , page: int
      }
  }

assetsPath = "assets" :: URL
assetsFilePath = "src/Frontend" :: URL
dependencyPath = "output" :: URL

htmlResponseHeader = header "Content-Type" "text/html" ∷ ResponseHeaders

homePagePath = "./index.html" :: FilePath

tokatHomeURL = "https://www.toplukatalog.gov.tr" :: URL

main :: ServerM
main = serve { port: 8080, onStarted } { route, router }
  where
  onStarted :: Effect Unit
  onStarted = Console.log "*** Book Catalog server started on http://127.0.0.1:8080 ***"

  router :: Request Route -> ResponseM
  router { route: Home } =
    liftEffect (Console.log "log Home") *> traceM "Home" *> readFile homePagePath >>= ok' htmlResponseHeader
  router { route: Asset asst, url } = case asst of
    PrimaryAsset name
      | ext <- extname name, isExtValid ext -> sendAsset [ "./", assetsFilePath ] name ext
    DependencyAsset pth name
      | ext <- extname name, isExtValid ext -> sendAsset [ "./", dependencyPath, pth ] name ext
    _ -> do
      liftEffect $ Console.log $ "path can not be found: \"" <> url <> "\""
      notFound
    where
    isExtValid = (_ `elem` [ ".js", ".css" ])
    getMime = case _ of
      ".js" -> "text/javascript"
      ".css" -> "text/css"
      ext -> unsafeCrashWith "Invalid extension (should never happen): \"" <> ext <> "\""
    contentHdr = header "Content-Type"
    sendAsset pths name ext = do
      let
        mime = getMime ext
        hdrs = contentHdr mime
        filePath = concat $ pths <> [ name ]
      liftEffect $ Console.log =<< ("cwd: \"" <> _) <<< (_ <> "\"") <$> cwd
      liftEffect $ Console.log $ "sending \"" <> filePath <> "\""
      readFile filePath >>= ok' hdrs

  router { route: TokatHome } = getTokatPage tokatHomeURL
  router
    { route: TokatFirstPage { _f, the_page, cwid, keyword, tokat_search_field, order, command } } = do
    let
      qry =
        { _f
        , the_page
        , cwid: show cwid
        , keyword
        , tokat_search_field: show tokat_search_field
        , order: show order
        , command
        }
    liftEffect $ Console.log $ "tokat-first-page query: " <> show qry
    getTokatPage $ prepTokatURL qry
  router { route: TokatNextPage { cwid, keyword, tokat_search_field, order, ts, page } } = do
    let
      qry =
        { cwid: show cwid
        , keyword
        , tokat_search_field: show tokat_search_field
        , order: show order
        , ts: show ts
        , page: show page
        }
    getTokatPage $ prepTokatURL qry

prepTokatURL :: forall r. { | r } -> String
prepTokatURL rec =
  let
    qry = unsafeCoerce rec :: Query
    qry' = toNullable $ Just $ toQueryString qry
  in
    format $ (parse tokatHomeURL) { search = qry' }

getTokatPage :: URL -> ResponseM
getTokatPage url = do
  ethResp <- get RF.string url
  liftEffect $ Console.log $ "getting page:" <> url
  case ethResp of
    Left err -> do
      liftEffect $ Console.log $ " error getting page " <> url <> "\n\terror: " <> printError err
      internalServerError $ printError err
    Right resp -> case resp.status of
      StatusCode 200 -> do
        liftEffect $ Console.log $ "successfully got " <> url
        let result = resp.body
        ok result
      StatusCode st -> do
        liftEffect $ Console.log $ "couldn't get page " <> url <> "\n\tstatus: " <> show st <> " " <> resp.statusText
        response misdirectedRequest (show st <> " " <> resp.statusText)
