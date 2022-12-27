module Frontend.BookProps where

import Prelude

import Affjax.ResponseFormat as RF
import Affjax.Web as AJ
import Common.BookProps (ISBN, BookProps)
import Control.Monad.Except (ExceptT(..), lift, runExceptT, withExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array (foldM, foldl, (:))
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), codePointFromChar, contains, null, takeWhile)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Frontend.Constants (tokatFirstPageURL, tokatHomeURL)
import Frontend.UI.UIUtils (selectElementsBy, selectNodesBy)
import Web.DOM (Element, Node)
import Web.DOM.DOMParser (makeDOMParser, parseHTMLFromString)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.Node as Node
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.DOM.ParentNode as ParentNode
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.HTMLOptionElement as HTMLOptionElement
import Web.HTML.HTMLSelectElement as HTMLSelectElement
import Web.HTML.HTMLTableRowElement as HTMLTableRowElement
import Web.HTML.Window (document)
import Web.URL as URL
import Web.URL.URLSearchParams as URLSearchParams

collectBookProperties :: ISBN -> Aff (Either String (BookProps))
collectBookProperties isbn = runExceptT do
  htmlParser <- lift $ liftEffect makeDOMParser
  pageDoc <- fetchPageAndParse htmlParser tokatHomeURL
  -- form <- lift $ liftEffect $ querySelector (QuerySelector "form[__the_form]") $ toParentNode pageDoc
  fpParamsMap <- Map.update (const $ Just isbn) "keyword" <$> getFirstPageParams pageDoc
  fpUrl <- paramsToUrl tokatFirstPageURL (Right fpParamsMap) -- pageDoc
  fpDoc <- fetchPageAndParse htmlParser fpUrl
  -- >>= Document.documentElement >>> liftEffect
  -- >>= note "could not find documentElement in collectBookProperties" >>> pure >>> ExceptT
  pageBookProperties Map.empty fpDoc
  --   fstPageProps <- pageBookProperties Map.empty fpDoc
  --   liftEffect $ Console.log $ "first page properties: " <> show fstPageProps
  --   links <- pageLinks fpDoc
  --   links' <- init links # note "no link could be found" # except
  --   -- pure $ liftEffect $ Console.log $ "first page links: " <> show links
  --   links' # flip foldM fstPageProps \props search -> do
  --     npUrl <- paramsToUrl tokatNextPageURL (Left $ Just search) -- pageDoc
  --     liftEffect $ Console.log $ "next page link: " <> npUrl
  --     npDoc <- fetchPageAndParse htmlParser npUrl
  --     pageBookProperties props npDoc
  where
  fetchPageAndParse parser url = do
    resp <- getPage url # ExceptT # withExceptT AJ.printError
    doc <- parseHTMLFromString resp.body parser # liftEffect # ExceptT
    Document.documentElement doc
      >>= note "could not find documentElement in collectBookProperties" >>> pure
      # liftEffect
      # ExceptT

  --   pageLinks :: forall m. MonadAff m => Element -> ExceptT String m (Array String)
  --   pageLinks doc = do
  --     linkEntries <- selectElementsBy "#page_number_links_top > a" doc
  --     for linkEntries doLink >>= sequence >>> pure # liftEffect # ExceptT
  --   doLink el = do
  --     let link = HTMLAnchorElement.toHTMLHyperlinkElementUtils <$> HTMLAnchorElement.fromElement el
  --     traverse (map (dropWhile (_ /= codePointFromChar '?')) <<< HTMLHyperlinkElementUtils.href) link
  --       >>= note "unable to convert Element to HTMLLinkElement in pageLinks" >>> pure
  pageBookProperties props doc = do
    libraryEntries <- selectElementsBy "#search_results > table td.result_headers_container tbody" doc
    libraryEntries # flip foldM props \libps tbody -> do
      propEntries <- selectNodesBy "tr" tbody
      foldM doSingleProp libps =<< liftEffect (NodeList.toArray propEntries)

  doSingleProp :: forall m. MonadAff m => BookProps -> Node -> ExceptT String m BookProps
  doSingleProp ps tr = do
    -- let
    --   tr' = HTMLTableRowElement.fromNode tr
    --     # note ("can not convert to HTMLRowElement in doSingleProp")
    key <- doSingleCell 0 tr -- =<< ExceptT (pure tr')
    let key' = if contains (Pattern ":") key then takeWhile (_ /= codePointFromChar ':') key else key
    val <- doSingleCell 1 tr -- =<< ExceptT (pure tr')
    pure $ ps # flip Map.alter key' case _ of
      Just vals -> Just $ val : vals
      Nothing -> Just $ Array.singleton val

  doSingleCell :: forall m. MonadAff m => Int -> Node -> ExceptT String m String
  doSingleCell idx tr = ExceptT $ liftEffect $ note ("toNode failed when getting td " <> show idx) <$> do
    let tds = traverse HTMLTableRowElement.cells $ HTMLTableRowElement.fromNode tr
    td <- runMaybeT $ map Element.toNode <<< MaybeT <<< HTMLCollection.item idx =<< MaybeT tds
    traverse Node.textContent td

paramsToUrl :: forall m. MonadAff m => AJ.URL -> Either (Maybe String) (Map String String) {- -> Document -} -> ExceptT String m AJ.URL
paramsToUrl relUrl params {- doc -} = ExceptT $ liftEffect $ do
  pageUrl <- (Document.url <<< HTMLDocument.toDocument =<< document =<< window)
  let
    absUrl = URL.fromRelative relUrl pageUrl
    urlQuery = URL.searchParams <$> absUrl
    urlQuery' = case params of
      Left mbqry -> case mbqry of
        Just qry -> Just $ URLSearchParams.fromString qry
        Nothing -> urlQuery
      Right list ->
        ( flip (foldl (\uq (Tuple k v) -> URLSearchParams.append k v uq))
            (Map.toUnfoldable list)
        ) <$> urlQuery
    urlWithQuery =
      URL.setSearch <$> map URLSearchParams.toString urlQuery' <*> absUrl
  URL.toString <$> urlWithQuery # note "can not append query to page url in paramsToUrl" # pure

getFirstPageParams :: forall m. MonadAff m => Element -> ExceptT String m (Map String String)
getFirstPageParams doc = do
  form <- (querySelector (QuerySelector "form[name=__the_form]") $ Element.toParentNode doc)
    <#> note ("Form not found in getFirstPageParams. selector: \"form[__the_form]\"")
    # liftEffect
    # ExceptT
  inputs <- selectNodesBy "input[type=hidden], input[type=text], input[type=submit]" form
  inpParams <- foldOverWith doInput inputs Map.empty
  selects <- selectNodesBy "select" form
  result <- foldOverWith doSelect selects inpParams
  liftEffect $ Console.log $ "first page params:" <> show result
  pure result
  where
  foldOverWith f nodes params = ExceptT $ liftEffect do
    arr <- NodeList.toArray nodes
    foldM f params arr <#> Right
  doInput ps nd = do
    let mbInp = HTMLInputElement.fromNode nd
    case mbInp of
      Just inp -> do
        name <- HTMLInputElement.name inp
        val <- HTMLInputElement.value inp
        pure $ Map.insert name val ps
      Nothing -> pure ps
  doSelect ps nd = do
    let mbSel = HTMLSelectElement.fromNode nd
    case mbSel of
      Just sel -> do
        name <- HTMLSelectElement.name sel
        val <- HTMLSelectElement.value sel
        if null val then do
          let pn = HTMLSelectElement.toParentNode sel
          mbOpt <- ParentNode.querySelector (QuerySelector "option[selected]") pn
            >>= map HTMLOptionElement.fromElement >>> join >>> pure
          case mbOpt of
            Just opt -> do
              val' <- HTMLOptionElement.value opt
              pure $ Map.insert name val' ps
            Nothing -> pure $ Map.insert name val ps
        else pure $ Map.insert name val ps
      Nothing -> pure ps

getPage :: AJ.URL -> Aff (Either AJ.Error (AJ.Response String))
getPage url = AJ.get RF.string url

