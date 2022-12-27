module Frontend.BookPropsInput where

import Prelude

import Common.BookProps (BookPropName, BookProps, SetBookProp, BookPropValues)
import Control.Alt ((<|>))
import Data.Array ((!!))
import Data.Array as Array
import Data.Filterable (compact, filterMap)
import Data.Foldable (any, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (null, trim) as String
import Data.String.Utils (lines) as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug (spy, spyWith)
import Deku.Attribute (cb, (!:=))
import Deku.Attributes (id_, klass, klass_)
import Deku.Control (blank, text, text_, (<#~>))
import Deku.Core (Domable, dyn, fixed)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useDyn_, useHot, useHot')
import Deku.Lifecycle (onDidMount)
import Deku.Listeners (checkbox, checkbox_, click, textInput, textInput_)
import Effect (Effect)
import Effect.Console as Console
import FRP.Event (Event, fold, mapAccum, withLast)
import Frontend.UI.UIUtils (checked, disabled, infoPanel, onBlur, onDblclick, turkCharsToEng, value, withInputElementValue)
import QualifiedDo.Alt as Alt
import Web.DOM.NonElementParentNode as NonElementParentNode
import Web.Event.Event as WebEvent
import Web.Event.Internal.Types (EventTarget)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)

data FormControlClass = LabelFor | FormControl | FormControlPlaintext

derive instance Generic FormControlClass _
derive instance Eq FormControlClass
instance Show FormControlClass where
  show s = genericShow s

bookPropsInputScene
  :: forall lock payload
   . BookProps
  -> String
  -> Domable lock payload
bookPropsInputScene props isbn = Deku.do
  setPropResult /\ propResult <- useHot'
  let
    result = fold
      (\ps (key /\ val) -> Map.alter (const $ if Array.null val then Nothing else Just val) key ps)
      Map.empty
      propResult
  -- getProp name = fromMaybe [] <<< Map.lookup name
  D.div_
    [ D.div (klass_ "row")
        [ D.h3 (klass_ "text-center offset-1 col-10") [ text_ isbn ]
        , D.button
            Alt.do
              klass_ "btn btn-primary col-1"
              disabled $ pure true <|> propsInvalid result
            -- click $ result <#> cb <<< const <<< setBookProps <<< Right
            [ text_ "Tamam" ]
        ]
    , D.div (klass_ "row")
        [ fixed $ radioKeys <#> \name -> radioPropInput name setPropResult $ Map.lookup name props
        , fixed $ checkboxKeys <#> \name -> checkboxPropInput name setPropResult $ Map.lookup name props
        ]
    ]
  where
  radioKeys = [ "Başlık", "Yazar", "Alt Yazar" ]
  checkboxKeys = [ "Konu" ]
  allKeys = Set.fromFoldable $ radioKeys <> checkboxKeys
  propsInvalid =
    ( _ <#> \props ->
        let
          keys = Map.keys props
          values = Map.values props
        in
          Set.difference allKeys keys /= Set.empty
            || any (\v -> Array.null v || v == [ "" ]) values
    )

  radioPropInput
    :: BookPropName
    -> SetBookProp
    -> Maybe BookPropValues
    -> Domable lock payload
  radioPropInput name setResult mbVals = Deku.do
    setLocal /\ local <- useHot ""
    let
      engPropName = turkCharsToEng name
      cls = fold (\_ s -> "form-control" <> if String.null s then " is-invalid" else "") "form-control" local
      setBoth v = setLocal v *> setResult (name /\ [ v ])
    D.fieldset (klass_ "col-6") -- (klass $ result <#> String.null >>> (if _ then " text-danger" else "") >>> ("col-6" <> _))
      [ D.legend_ [ text_ name ]
      , D.input
          Alt.do
            klass cls
            D.Xtype !:= "text"
            id_ $ engPropName <> "-input"
            D.Placeholder !:= name <> " seçiniz ve/ya da giriniz"
            value local
            D.Required !:= show true
            -- textChange $ setResult 
            textInput_ setBoth
          []
      , case mbVals of
          Nothing -> infoTokat name
          Just vals ->
            fixed $ Array.nub vals # Array.mapWithIndex \idx s ->
              let
                id = engPropName <> show idx
              in
                D.div (klass_ "form-check")
                  [ D.input
                      Alt.do
                        klass_ "form-check-input"
                        D.Xtype !:= "radio"
                        D.Name !:= engPropName
                        D.Id !:= id
                        checked $ local <#> (_ == s)
                        checkbox_ \chk -> when chk $ setBoth s
                      []
                  , D.label (klass_ "form-check-label" <|> D.For !:= id) [ text_ s ]
                  ]
      ]

  checkboxPropInput
    :: String
    -> SetBookProp
    -> Maybe BookPropValues
    -> Domable lock payload
  checkboxPropInput name setResult mbVals = Deku.do
    let engName = turkCharsToEng name
    D.fieldset (klass_ "col-6 row")
      [ D.legend_ [ text_ name ]
      , case mbVals of
          Nothing -> infoTokat name
          Just vals ->
            let
              valsUnique = vals
                <#> String.lines >>> map String.trim >>> Array.filter (not <<< String.null)
                # Array.concat
                # Array.nub
                # Array.sort
              lenUnique = Array.length valsUnique
              nothArray len = Array.replicate len Nothing
            in
              Deku.do
                setLocal /\ local <- useHot $ nothArray lenUnique
                let
                  setLocalFunc :: Int -> Array (Maybe String) -> Maybe String -> Effect Unit
                  setLocalFunc idx arr val = do
                    let
                      len = Array.length arr
                      arr' = if idx >= len then arr <> nothArray (idx - len + 1) else arr
                    case Array.modifyAt idx (const val) arr' of
                      Nothing -> Console.log $ "Index out of bounds modifying array with " <> show val <> " at " <> show idx
                      Just newArr -> do
                        setLocal newArr
                        setResult (name /\ compact newArr)

                  setLocalE :: Int -> Event (Maybe String -> Effect Unit)
                  setLocalE idx = local <#> (\arr -> setLocalFunc idx arr)
                  -- checkLocal idx = local <#> (_ `unsafePartial Array.unsafeIndex` idx)
                  changeFilter idx evt = withLast (evt <#> (_ !! idx) >>> join) {- `unsafePartial Array.unsafeIndex` idx)) -} 
                    # filterMap (\{ last, now } -> if last == Just now then Nothing else Just now)
                  noopRemove = pure unit
                fixed
                  [ fixed $ valsUnique # Array.mapWithIndex
                      \idx s -> checkControl engName
                        (setLocalE idx)
                        (changeFilter idx local)
                        noopRemove
                        idx
                        s
                  , Deku.do
                      setItem /\ item <- useHot'
                      fixed
                        [ D.div (klass_ "col-6")
                            [ D.a
                                Alt.do
                                  klass_ "fs-4 fw-bold"
                                  D.OnClick !:= cb \_ -> setItem unit
                                [ D.i (klass_ "text-primary bi-plus-circle-fill") [] ]
                            ]
                        , dyn $ mapAccum (\i _ -> Tuple (i + 1) i) lenUnique item <#> \idx -> Deku.do
                            { remove } <- useDyn_
                            let
                              setLocE = setLocalE idx
                              chgFilt = changeFilter idx local
                              remove' = setLocE <#> \setl -> setl Nothing *> remove
                            D.div (klass_ "row")
                              [ D.a
                                  Alt.do
                                    D.Class !:= "col-auto"
                                    click remove'
                                  [ D.i (klass_ "text-danger bi-dash-circle-fill") [] ]
                              , D.div (klass_ "col")
                                  [ checkControl engName setLocE chgFilt remove idx ""
                                  ]
                              ]
                        ]
                  ]
      ]
    where
    checkControl
      :: String
      -> Event (Maybe String -> Effect Unit)
      -> Event (Maybe String)
      -> Effect Unit
      -> Int
      -> String
      -> Domable lock payload
    checkControl engName setLocal local remove idx hdr = Deku.do
      let
        added = String.null hdr
        id = engName <> show (idx :: Int)
        hdrId = id <> "-hdr" <> if added then "-added" else ""
      setChk /\ chk <- useHot added
      -- setTxt /\ txt <- useState Nothing
      setCls /\ cls <- useHot if added then FormControl else LabelFor
      let prevLoc = withLast local <#> \{ last } -> join last

      fixed
        [ D.div (klass_ $ "form-check" <> if added then "" else " col-6")
            [ ( if added then blank
                else
                  D.input
                    Alt.do
                      klass_ "form-check-input"
                      D.Xtype !:= "checkbox"
                      D.Name !:= id
                      D.Id !:= id
                      checked chk
                      checkbox $
                        ( \mbVal ch setl ch' -> when (ch /= ch') do
                            setChk ch'
                            let
                              proposedVal =
                                if ch' then case mbVal of
                                  Nothing -> if String.null hdr then Nothing else Just hdr
                                  _ -> mbVal
                                else Nothing
                            when (proposedVal /= mbVal) $ setl proposedVal
                        ) <$> local <*> chk <*> setLocal -- \mbVal setl ch -> setl mbVal *> setChk ch
                    []
              ) <>
                ( cls <#~> case _ of
                    LabelFor -> D.label
                      Alt.do
                        klass_ "form-check-label w-100"
                        D.For !:= id
                        onDblclick $
                          ( \curVal prevVal setl -> do
                              if added && curVal /= prevVal && isJust prevVal then
                                setl prevVal
                              else pure unit
                              setCls FormControl
                              setChk true
                          ) <$> local <*> prevLoc <*> setLocal
                      [ text $ local <#> case _ of
                          Nothing -> hdr
                          Just t -> t
                      ]
                    _ -> onDidMount (when added $ setFocus hdrId)
                      $ D.input
                          Alt.do
                            D.Id !:= hdrId
                            D.Xtype !:= "text"
                            klass_ "form-control p-0"
                            D.Placeholder !:= "Konu giriniz"
                            value $ local <#> case _ of
                              Nothing -> hdr
                              Just val -> val
                            onDblclick $ setLocal <#> \setl -> cb
                              ( flip withInputElementValue \v ->
                                  if added && String.null v then setl Nothing *> remove
                                  else setCls LabelFor
                              )
                            onBlur $ setLocal <#> \setl -> cb
                              ( flip withInputElementValue \v ->
                                  if added && String.null v then setl Nothing *> remove
                                  else setCls LabelFor
                              )
                            textInput $
                              ( \mbVal ch setl val -> do
                                  Console.log $ "textInput:" <> show mbVal <> "," <> val
                                  when (ch && mbVal /= Just val) $
                                    setl if String.null val then Nothing else Just val
                              ) <$> local <*> chk <*> setLocal
                          []
                )
            ]
        ]
    setFocus hdrId = window
      >>= document
      <#> HTMLDocument.toNonElementParentNode
      >>= NonElementParentNode.getElementById hdrId
      <#> (_ >>= HTMLElement.fromElement)
      >>= traverse_ HTMLElement.focus

eventTargetTo :: forall html. (EventTarget -> Maybe html) -> WebEvent.Event -> Maybe html
eventTargetTo fromTarget = WebEvent.target >=> fromTarget

infoTokat :: forall lock payload. String -> Domable lock payload
infoTokat name = infoPanel $ name <> " Toplu Katalog'dan gelen kitap bilgilerinin arasında bulunamadı"
