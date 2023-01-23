module Frontend.UI.UIUtils where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..))
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Foldable (for_)
import Data.Foldable as Foldable
import Data.Maybe (fromJust, isJust)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Deku.Attribute (class Attr, Attribute, cb, (!:=), (:=))
import Deku.Control (text_)
import Deku.Core (Domable)
import Deku.DOM as D
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import FRP.Event (class IsEvent, Event, keepLatest, sampleOnRight)
import Partial.Unsafe (unsafePartial)
import Web.DOM (Element, NodeList)
import Web.DOM.Element as Element
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.Event.Event as WebEvent
import Web.HTML.HTMLInputElement (HTMLInputElement)
import Web.HTML.HTMLInputElement as HTMLInputElement

errorPanel :: forall lock payload. String -> Domable lock payload
errorPanel msg = Deku.do
  D.div (D.Class !:= "alert alert-danger" <|> D.Role !:= "alert") [ text_ msg ]

infoPanel :: forall lock payload. String -> Domable lock payload
infoPanel msg = Deku.do
  D.div (D.Class !:= "alert alert-info" <|> D.Role !:= "info") [ text_ msg ]

sameIdAndName :: forall elem. Attr elem D.Id String => Attr elem D.Name String => String -> Event (Attribute elem)
sameIdAndName c = D.Id !:= c <|> D.Name !:= c

attrToFuncE :: forall e attr a val. Attr e attr val => attr -> (a -> val) -> Event a -> Event (Attribute e)
attrToFuncE attr conv = map ((attr := _) {- <<< spyWith "attrToFunc" identity  -} <<< conv)

attrToFuncEIdentity :: forall e attr a. Attr e attr a => attr -> Event a -> Event (Attribute e)
attrToFuncEIdentity = flip attrToFuncE identity

attrToFuncEShow :: forall e attr a. Attr e attr String => Show a => attr -> Event a -> Event (Attribute e)
attrToFuncEShow = flip attrToFuncE show

-- toggleAttr :: forall a attr e. Attr e attr a => attr -> Event Boolean -> Event (Attribute e)
-- toggleAttr attr = map (if _ then attr := "true" else attr := unit)

-- attrToFuncMaybe
--   :: forall e attr a
--    . Attr e attr a
--   => attr
--   -> Event (Maybe a)
--   -> Event (Attribute e)
-- attrToFuncMaybe attr = map
--   ( case _ of
--       Nothing -> empty
--       Just val -> attr := val
--   )

disabled ∷ forall e. Attr e D.Disabled String => Event Boolean -> Event (Attribute e)
disabled = attrToFuncEShow D.Disabled -- show 

readonly ∷ forall e. Attr e D.Readonly String => Event Boolean -> Event (Attribute e)
readonly = map (if _ then D.Readonly := "true" else D.Readonly := unit)

checked ∷ forall e. Attr e D.Checked String => Event Boolean -> Event (Attribute e)
checked = attrToFuncEShow D.Checked

-- checked = map ((D.Checked := _) <<< show)

value ∷ forall e. Attr e D.Value String => Event String -> Event (Attribute e)
value = attrToFuncEIdentity D.Value -- map ((D.Value := _))

onDblclick ∷ forall a e. Attr e D.OnDblclick a => Event a -> Event (Attribute e)
onDblclick = attrToFuncEIdentity D.OnDblclick -- map ((D.Value := _))

onFocus ∷ forall a e. Attr e D.OnFocus a => Event a -> Event (Attribute e)
onFocus = attrToFuncEIdentity D.OnFocus -- map ((D.Value := _))

onBlur ∷ forall a e. Attr e D.OnBlur a => Event a -> Event (Attribute e)
onBlur = attrToFuncEIdentity D.OnBlur -- map ((D.Value := _))

zipOnRight ∷ forall event a b. IsEvent event ⇒ event a → event b → event (Tuple a b)
zipOnRight ev1 ev2 = sampleOnRight ev1 $ ev2 <#> \e2 e1 -> Tuple e1 e2

zip ∷ forall event b a. IsEvent event ⇒ event a → event b → event (Tuple a b)
zip ev1 ev2 = (ev1 <#> \e1 -> ev2 <#> \e2 -> Tuple e1 e2) # keepLatest

textChange
  :: forall e
   . Event (String -> Effect Unit)
  -> Event (Attribute e)
textChange = map \push -> D.OnChange := cb (flip withInputElementValue push) -- \e -> for_

-- (WebEvent.target e >>= HTMLInputElement.fromEventTarget)
-- (HTMLInputElement.value >=> push)

turkCharsToEng ∷ String → String
turkCharsToEng =
  let
    turkishChars = toCharArray "ğüşıöçĞÜŞİÖÇ"
    englishChars = toCharArray "güsiocGUSIOC"
    turEngPair = Array.zip turkishChars englishChars
  in
    toCharArray
      >>> map
        ( \x ->
            let
              mb = Foldable.lookup x turEngPair
            in
              if isJust mb then unsafePartial $ fromJust mb else x
        )
      >>> fromCharArray

withInputElement ∷ WebEvent.Event → (HTMLInputElement → Effect Unit) → Effect Unit
withInputElement evt f = for_ (WebEvent.target evt >>= HTMLInputElement.fromEventTarget) f

withInputElementValue ∷ WebEvent.Event → (String → Effect Unit) → Effect Unit
withInputElementValue evt f = withInputElement evt $ HTMLInputElement.value >=> f

selectElementsBy :: forall m. MonadAff m => String -> Element -> ExceptT String m (Array Element)
selectElementsBy query el =
  do
    nodes <- selectNodesBy query el
    NodeList.toArray nodes
      <#> traverse Element.fromNode
        >>> note "can not convert to Element array in selectElementsBy"
      # liftEffect
      # ExceptT

-- ( traverse Element.fromNode >>>
--     note "can not convert to Element array in selectElementsBy"
-- ) <$> NodeList.toArray nodes # liftEffect -- # ExceptT

selectNodesBy :: forall m. MonadAff m => String -> Element -> ExceptT String m NodeList
selectNodesBy query form = ExceptT $ liftEffect do
  nodes <- querySelectorAll (QuerySelector query) (Element.toParentNode form)
  len <- NodeList.length nodes
  pure $
    if len == 0 then Left $ "Nodes not found in selectNodesBy. selector: \"" <> query <> "\""
    else Right nodes
