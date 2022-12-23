module Frontend.BookPropsInput where

import Prelude

import Bolson.Core (Entity(..), FixedChildren(..))
import Control.Alt ((<|>))
import Control.Monad.Except (runExceptT)
import Control.Plus (class Plus)
import Control.Plus as Plus
import Data.Array ((!!), (:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Filterable (compact, filter, filterMap)
import Data.Foldable (any, for_, traverse_)
import Data.Foldable as Foldable
import Data.Functor (mapFlipped)
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, maybe)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Utils as String
import Data.Traversable (for, sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Debug (spy, spyWith)
import Deku.Attribute (Attribute, cb, cb', maybeAttr, (!:=), (:=), (?:=))
import Deku.Attributes (id_, klass, klass_)
import Deku.Control (blank, text, text_, (<#~>))
import Deku.Core (Domable(..), bus, dyn, fixed, remove)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useDyn_, useHot, useHot', useState, useState')
import Deku.Lifecycle (onDidMount, onWillMount)
import Deku.Listeners (checkbox, checkbox_, click, click_, textInput, textInput_)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import FRP.Behavior (step)
import FRP.Event (Event, create, fold, gate, keepLatest, mapAccum, subscribe, withLast)
import FRP.Event.Aff (bindToAffWithCancellation)
import FRP.Event.Class ((*|>), (<**>), (<*|>), (<|*>))
import Frontend.BookProps (BookPropName, BookPropValue, BookProps, BookPropValues)
import Frontend.UI.UIUtils (checked, disabled, errorPanel, infoPanel, onBlur, onDblclick, onFocus, readonly, selectElementsBy, textChange, turkCharsToEng, value, withInputElement, withInputElementValue, zip)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import QualifiedDo.Alt as Alt
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.Element as Element
import Web.DOM.NodeList as NodeList
import Web.DOM.NonElementParentNode as NonElementParentNode
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode as ParentNode
import Web.Event.Event as Event
import Web.Event.Event as WebEvent
import Web.Event.Internal.Types (EventTarget)
import Web.HTML (HTMLDivElement, window)
import Web.HTML.Event.EventTypes (offline)
import Web.HTML.HTMLDivElement as HTMLDivElement
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement (HTMLInputElement)
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.Window (document)

type BookProp = BookPropName /\ BookPropValues

type SetBookProp = BookProp -> Effect Unit

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
          keys = Map.keys $ spyWith "props" show props
          values = Map.values props
        in
          Set.difference allKeys keys /= Set.empty
            || any (\v -> Array.null v || v == [ "" ]) values
    )

  radioPropInput
    :: {- forall lock payload
    . -} BookPropName
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
    :: {-  forall lock payload
    . -} String
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
                    case Array.modifyAt idx (const val) (spy "arr" arr') of
                      Nothing -> Console.log $ "Index out of bounds modifying array with " <> show val <> " at " <> show idx
                      Just newArr -> do
                        setLocal (spy "newArr" newArr)
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
      :: forall lock payload
       . String
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
        maybeVal val = if String.null val then Nothing else Just val
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
                          Nothing -> {- spy "hdr" -}  hdr
                          Just t -> {- spy "t" -}  t
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

-- clsToStr = case _ of
--   FormControl -> "form-control p-0"
--   FormControlPlaintext -> "form-control-plaintext p-0"
-- swapCls = case _ of
--   FormControl -> FormControlPlaintext
--   FormControlPlaintext -> FormControl

eventTargetTo :: forall html. (EventTarget -> Maybe html) -> WebEvent.Event -> Maybe html
eventTargetTo fromTarget = WebEvent.target >=> fromTarget

infoTokat :: forall lock payload. String -> Domable lock payload
infoTokat name = infoPanel $ name <> " Toplu Katalog'dan gelen kitap bilgilerinin arasında bulunamadı"

-- bookPropsInputScene'
--   :: forall lock payload
--    . { setBookProps :: Either String BookProps -> Effect Unit
--      , props :: BookProps
--      , isbn :: String
--      , setResultDisplay :: Boolean -> Effect Unit
--      }
--   -> Domable lock payload
-- bookPropsInputScene' { setBookProps, props, isbn, setResultDisplay } = Deku.do
--   --   setHeader /\ header <- useState'
--   --   setHeaderInputValue /\ headerInputValue <- useState'
--   setBookProps' /\ bookProps' <- useState $ Map.empty
--   let
--     radioKeys = [ "Başlık", "Yazar", "Alt Yazar" ]
--     checkboxKeys = [ "Konu" ]
--     allKeys = Set.fromFoldable $ radioKeys <> checkboxKeys
--     propsInvalid = bookProps' <#> \props ->
--       let
--         keys = Map.keys props
--         values = Map.values props
--       in
--         Set.difference allKeys keys /= Set.empty
--           || not (List.null $ List.filter (\v -> Array.null v || v == [ "" ]) values)

--   {- onDidMount ((subscribe bookProps' \p -> Console.log $ "props on info scene: " <> show p) *> pure unit) $ -}
--   D.div_
--     [ D.div (klass_ "row")
--         [ D.h3 (klass_ "text-center offset-1 col-10") [ text_ isbn ]
--         , D.button
--             Alt.do
--               klass_ "btn btn-primary col-1"
--               disabled propsInvalid
--               click $ bookProps' <#> cb <<< const <<< setBookProps <<< Right
--             [ text_ "Tamam" ]
--         ]
--     , D.div (klass_ "row")
--         ( ([ "Başlık", "Yazar", "Alt Yazar" ] # map \pn -> radioPropInput pn setBookProps' bookProps' $ Map.lookup pn props)
--             <> ([ "Konu" ] # map \pn -> checkboxPropInput pn setBookProps' bookProps' $ Map.lookup pn props)
--         )
--     ]

-- radioPropInput'
--   ∷ forall lock payload
--    . String
--   -> SetBookProp
--   -> GetBookProps
--   -> Maybe (Array String)
--   -> Domable lock payload
-- radioPropInput' propName setBookProps bookProps mbArr = Deku.do
--   let engPropName = turkCharsToEng propName
--   setResult /\ result' <- useHot ""
--   let result = (\{ now } -> now) <$> (filter (\{ last, now } -> last /= Just now) $ withLast result')
--   onDidMount
--     ( do
--         _ <- subscribe ((result <#> \s prop -> Map.alter (const $ Just [ s ]) propName prop) <|*> bookProps) setBookProps
--         pure unit
--     ) $
--     D.fieldset (klass_ "col-6") -- (klass $ result <#> String.null >>> (if _ then " text-danger" else "") >>> ("col-6" <> _))
--       [ D.legend_ [ text_ propName ]
--           <> case mbArr of
--             Nothing -> errorPanel $ propName <> " kitap bilgilerinin arasında bulunamadı"
--             Just arr ->
--               D.input
--                 Alt.do
--                   klass $ result <#> String.null >>> (if _ then " is-invalid" else "") >>> ("form-control" <> _)
--                   D.Xtype !:= "text"
--                   id_ $ engPropName <> "-input"
--                   D.Placeholder !:= propName <> " seçiniz ve/ya da giriniz"
--                   value result
--                   D.Required !:= show true
--                   textInput_ setResult
--                 []
--                 <> fixed
--                   ( Array.nub arr # Array.mapWithIndex \idx s ->
--                       let
--                         id = engPropName <> show idx
--                       in
--                         D.div (klass_ "form-check")
--                           [ D.input
--                               Alt.do
--                                 klass_ "form-check-input"
--                                 D.Xtype !:= "radio"
--                                 D.Name !:= engPropName
--                                 D.Id !:= id
--                                 checked $ result <#> (_ == s)
--                                 checkbox_ \chk -> when chk $ setResult s
--                               []
--                           , D.label (klass_ "form-check-label" <|> D.For !:= id) [ text_ s ]
--                           ]
--                   )
--       ]

-- checkboxPropInput'
--   ∷ forall lock payload
--    . String
--   -> SetBookProps
--   -> GetBookProps
--   → Maybe (Array String)
--   → Domable lock payload
-- checkboxPropInput' propName setBookProps bookProps mbArr = Deku.do
--   let engPropName = turkCharsToEng propName
--   setResultTick /\ resultTick <- useHot'
--   setItem /\ item <- useState'
--   D.fieldset (klass_ "col-6 row")
--     [ D.legend_ [ text_ propName ]
--         <> case mbArr of
--           Nothing -> errorPanel $ propName <> " kitap bilgilerinin arasında bulunamadı"
--           Just arr ->
--             let
--               arrUnique = arr
--                 <#> String.lines >>> map String.trim >>> Array.filter (not <<< String.null)
--                 # Array.concat
--                 # Array.nub
--                 # Array.sort
--             in
--               Deku.do
--                 setCbxGroup /\ (cbxGroup :: Event HTMLDivElement) <- useHot'
--                 let
--                   collectTopics = do
--                     _ <- subscribe
--                       (resultTick *|> (bookProps <#> (\p g -> g /\ p)) <*|> cbxGroup)
--                       \(parent /\ props) -> liftEffect $ do
--                         let el = HTMLDivElement.toParentNode parent
--                         els <- NodeList.toArray
--                           =<< ParentNode.querySelectorAll (QuerySelector ".form-check > .form-check-input:checked + input") el
--                         let inputs = traverse HTMLInputElement.fromNode els
--                         headers <- sequence $ traverse HTMLInputElement.value <$> inputs
--                         let
--                           headers' = maybe Nothing
--                             ( \hs ->
--                                 let
--                                   fhs = Array.filter (not <<< String.null <<< String.trim) hs
--                                 in
--                                   if Array.null fhs then Nothing else Just hs
--                             )
--                             headers
--                         Console.log $ "headers: " <> show headers'
--                         setBookProps $ Map.alter (const headers') propName props
--                     pure unit
--                   sendTick = setResultTick unit
--                 onDidMount collectTopics $ D.div (D.SelfT !:= setCbxGroup) -- fixed
--                   [ fixed $ arrUnique # Array.mapWithIndex \idx s -> checkControl sendTick (pure unit) idx s
--                   , D.div (klass_ "col-6")
--                       [ D.a
--                           Alt.do
--                             klass_ "fs-4 fw-bold"
--                             D.OnClick !:= cb \_ -> setItem unit
--                           [ D.i (klass_ "text-primary bi-plus-circle-fill") [] ]
--                       ]
--                   , dyn $ item <#> \_ -> Deku.do
--                       { remove } <- useDyn_
--                       D.div (klass_ "row")
--                         -- [ D.button
--                         --     Alt.do
--                         --       D.Class !:= "btn btn-danger rounded-circle btn-sm col-auto"
--                         --       click_ remove
--                         --     [ D.i (klass_ "dash-circle-fill") [] ]
--                         [ D.a
--                             Alt.do
--                               D.Class !:= "col-auto"
--                               click_ remove
--                             [ D.i (klass_ "text-danger bi-dash-circle-fill") [] ]
--                         , D.div (klass_ "col") [ checkControl sendTick remove 0 "" ]
--                         ]
--                   ]
--     ]
--   where
--   checkControl sendTick remove idx s = Deku.do
--     let
--       added = String.null s
--       addedCls cls = if added then " " <> cls else ""
--       addedCol = addedCls "col"
--       addedColAuto = addedCls "col-auto"
--       id = propName <> show (idx :: Int)
--       hdrId = id <> "-hdr" <> if added then "-added" else ""
--       ctlClsToStr = case _ of
--         FormControl -> "form-control p-0"
--         FormControlPlaintext -> "form-control-plaintext p-0"
--     -- swapCls = case _ of
--     --   FormControl -> FormControlPlaintext
--     --   FormControlPlaintext -> FormControl
--     -- chooseCls = case _ of
--     --   false -> FormControlPlaintext
--     --   true -> FormControl
--     setChk /\ chk <- useHot false
--     -- setLabelInput /\ (labelInput :: Event HTMLInputElement) <- useHot'
--     setCtlCls /\ ctlCls <- useHot FormControlPlaintext
--     -- resultEvt@(setResult /\ (result :: Event String)) <- useHot'
--     let chkCls = zip chk ctlCls
--     fixed
--       [ D.div (klass_ $ "form-check" <> if added then "" else " col-6")
--           [ D.input
--               Alt.do
--                 klass_ "form-check-input"
--                 D.Xtype !:= "checkbox"
--                 D.Name !:= id
--                 D.Id !:= id
--                 D.Placeholder !:= "Konu giriniz"
--                 checked chk
--                 checkbox_ \ch -> setChk ch *> sendTick
--               []
--           , onDidMount (when (spyWith "added" show added) $ setFocus hdrId) $ D.input
--               Alt.do
--                 D.Id !:= hdrId
--                 D.Xtype !:= "text"
--                 klass $ chkCls <#> \(ch /\ cl) -> ctlClsToStr (if ch then cl else FormControlPlaintext)
--                 D.Value !:= s
--                 click $ chkCls <#> \(ch /\ cl) -> when (cl == FormControlPlaintext) $ setChk (not ch)
--                 -- D.Autofocus !:= show true
--                 onFocus $ chk <#> \ch -> do
--                   setCtlCls FormControl
--                   when (not ch) $ setChk true
--                 D.OnBlur !:= cb
--                   ( flip withInputElementValue \v ->
--                       if added && String.null v then remove *> sendTick
--                       else setCtlCls FormControlPlaintext
--                   )
--                 textInput_ $ const sendTick
--               -- when (not ch) $ setChk true
--               -- D.OnChange !:= cb (flip withInputElementValue \v -> when (added && String.null v) remove)
--               -- onDblclick $ chkCls <#> \(ch /\ cl) -> cb \evt -> do
--               --   withInputElementValue evt \v -> do
--               --     if added && String.null v then remove
--               --     else do
--               --       setCtlCls $ swapCls cl
--               --       when (not ch) $ setChk true
--               -- D.SelfT !:= setLabelInput
--               []
--           ]
--       ]
--   setFocus hdrId = window
--     >>= document
--     <#> HTMLDocument.toNonElementParentNode
--     >>= NonElementParentNode.getElementById hdrId
--     <#> (_ >>= HTMLElement.fromElement)
--     >>= traverse_ HTMLElement.focus

