module Frontend.BookPropsInput where

import Prelude

import Common.BookProps (BookPropName, BookPropValues, BookProps, SetBookProp, BookPropValue)
import Control.Alt ((<|>))
import Control.Monad.Except (runExceptT)
import Data.Array ((!!), (:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Filterable (compact, filterMap)
import Data.Foldable (any, for_, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (joinWith, null, trim) as String
import Data.String.Utils (lines) as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug (spyWith, trace)
import Deku.Attribute (cb, (!:=))
import Deku.Attributes (id_, klass, klass_)
import Deku.Control (blank, text, text_, (<#~>))
import Deku.Core (Domable, dyn, fixed)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useDyn_, useHot, useHot')
import Deku.Lifecycle (onDidMount, onWillMount)
import Deku.Listeners (checkbox, checkbox_, click, click_, textInput, textInput_)
import Effect (Effect)
import Effect.Aff (joinFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import FRP.Event (Event, fold, mapAccum, withLast)
import FRP.Event.Class ((<|**>))
import FRP.Event.Time (interval)
import Frontend.BookOperations (BookOpResult(..), addBook, deleteBook, updateBook)
import Frontend.UI.UIUtils (checked, disabled, infoPanel, onBlur, onDblclick, turkCharsToEng, value, withInputElementValue)
import Partial.Unsafe (unsafeCrashWith)
import QualifiedDo.Alt as Alt
import Web.DOM.NonElementParentNode as NonElementParentNode
import Web.Event.Event as WebEvent
import Web.Event.Internal.Types (EventTarget)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)

data Operation = Add | Update | Delete | Terminate

derive instance Generic Operation _
derive instance Eq Operation
instance Show Operation where
  show s = genericShow s

data OperationStatus
  = Idle
  | Operating
  | Operated
  | Errored String

data FormControlClass = LabelFor | FormControl | FormControlPlaintext

derive instance Generic FormControlClass _
derive instance Eq FormControlClass
instance Show FormControlClass where
  show s = genericShow s

bookPropsInputScene
  :: forall lock payload
   . BookProps
  -> Maybe BookProps
  -> String
  -> Domable lock payload
bookPropsInputScene props mbBookProps isbn = Deku.do
  setPropResult /\ propResult <- useHot'
  -- setDisable /\ disable <- useHot false
  setOperation /\ operation <- useHot case mbBookProps of
    Nothing -> Add
    Just _ -> Update
  setOperationStatus /\ _operationStatus_ <- useHot Idle
  let
    statusChange = withLast operation <#> case _ of
      { last: Just Update, now: Delete } -> Operating
      _ -> Idle
    operationStatus = statusChange <|> _operationStatus_
    result = fold
      (\ps (key /\ val) -> Map.alter (const $ if Array.null val then Nothing else Just val) key ps)
      ( case mbBookProps of
          Nothing -> trace "empty result" \_ -> Map.empty
          Just ps -> spyWith "full book" show ps
      )
      propResult
    disable = operationStatus <#> case _ of
      Idle -> false
      _ -> true
    ingIndicator opr =
      let
        (indicateText /\ color) = case opr of
          Add -> "Ekleniyor " /\ "primary"
          Update -> "Güncelleniyor " /\ "success"
          Delete -> "Siliniyor " /\ "warning"
          Terminate -> "Bu mesaj görünmemeli (ingIndicator Terminate)" /\ "dark"
      in
        D.div (klass_ $ "btn btn-" <> color <> " col-2")
          [ D.p_
              [ text_ indicateText
              , Deku.do
                  let
                    animClasses = [ "top", "split", "bottom" ] <#> ("bi-hourglass-" <> _)
                    anim = mapAccum (\i _ -> Tuple ((i + 1) `mod` 3) (animClasses # Array.drop i # Array.take 1 # String.joinWith "")) 0 (interval 100)
                  D.i (klass anim) []
              ]
          ]
    edIndicator opr =
      let
        (indicateText /\ color) = case opr of
          Add -> "Eklendi " /\ "primary"
          Update -> "Güncellendi " /\ "success"
          Delete -> "Silindi " /\ "warning"
          Terminate -> "Bu mesaj görünmemeli (edIndicator Terminate)" /\ "dark"
      in
        D.div (klass_ $ "alert alert-" <> color)
          [ D.p (klass_ "fs-4") [ D.i (klass_ "bi_check-circle-fill") [] ]
          , D.p_ [ text_ indicateText ]
          , D.a
              Alt.do
                klass_ "alert-link"
                click_ $ cb \_ -> do
                  setOperationStatus Idle
                  case opr of
                    Delete -> setOperation Terminate
                    Add -> setOperation Terminate
                    _ -> pure unit
              [ text_ "Devam etmek için tıklayınız..." ]
          ]
    errIndicator opr msg =
      let
        indicateText = case opr of
          Add -> "Eklenirken hata"
          Update -> "Güncellenirken hata"
          Delete -> "Silinirken hata"
          Terminate -> "Bu mesaj görünmemeli (errIndicator Terminate)"
      in
        D.div (klass_ $ "alert alert-danger")
          [ D.p (klass_ "fs-4") [ D.i (klass_ "bi_exclamation-triangle-fill") [] ]
          , D.p_ [ text_ $ indicateText <> ":" ]
          , D.p_ [ text_ msg ]
          , D.a
              Alt.do
                klass_ "alert-link"
                click_ $ cb \_ -> do
                  setOperationStatus Idle
                  case opr of
                    Delete -> setOperation Update
                    _ -> pure unit
              [ text_ "Devam etmek için tıklayınız..." ]
          ]

  -- getProp name = fromMaybe [] <<< Map.lookup name
  withLast operation <#~> case _ of
    { now: Terminate } -> blank
    { last } -> Deku.do
      onWillMount
        ( when (isNothing last) $ for_
            ( case mbBookProps of
                Nothing -> []
                Just b -> Map.toUnfoldable b
            )
            setPropResult
        ) $
        fixed
          [ D.div (klass_ "col-12")
              [ ((/\) <$> operation <*> operationStatus) <#~> case _ of
                  (_ /\ Idle) -> blank
                  (opr /\ Operating) -> ingIndicator opr
                  (opr /\ Operated) -> edIndicator opr
                  (opr /\ Errored msg) -> errIndicator opr msg
              , D.fieldset
                  Alt.do
                    klass $ disable <#> (if _ then " disabled" else "") >>> ("row" <> _)
                    disabled disable
                  [ D.div (klass_ "row")
                      [ D.h3 (klass_ "text-center offset-sm-2 col-sm-8") [ text_ isbn ]
                      , Deku.do
                          operation <#~> case _ of
                            Add -> D.button
                              Alt.do
                                klass_ "btn btn-primary col-sm-1"
                                disabled $ pure true <|> propsInvalid result
                                click $ result <#> \res -> cb \_ -> do
                                  setOperationStatus Operating
                                  fbr <- launchAff $ runExceptT $ addBook isbn res
                                  launchAff_ do
                                    r <- joinFiber fbr
                                    liftEffect $ setOperationStatus case r of
                                      Left err -> Errored $ "Hata:" <> err
                                      Right Conflict -> Errored "Kitap zaten eklenmiş!"
                                      Right _ -> Operated

                              [ text_ "Ekle" ]
                            Terminate -> blank -- oprIndicator "Ekleniyor" "primary"
                            _ {- | opr == Update || opr == Delete -} -> D.div
                              Alt.do
                                klass $ disable <#> \d -> "btn-group col-sm-2" <> if d then " disabled" else ""
                                D.Role !:= "group"
                              -- D.AriaLabel !:= "Action buttons"
                              [ D.button
                                  Alt.do
                                    D.Class !:= "btn btn-success"
                                    D.Xtype !:= "button"
                                    click $ result <#> \res -> cb \_ -> do
                                      setOperationStatus Operating
                                      fbr <- launchAff $ runExceptT $ updateBook isbn res
                                      launchAff_ do
                                        r <- joinFiber fbr
                                        liftEffect $ setOperationStatus case r of
                                          Left err -> Errored $ "Hata:" <> err
                                          Right NotFound -> Errored "Kitap bulunamadı!"
                                          Right _ -> Operated
                                  [ text_ "Düzelt" ]
                              , D.button
                                  Alt.do
                                    D.Class !:= "btn btn-warning"
                                    D.Xtype !:= "button"
                                    click_ $ cb \_ -> do
                                      -- setOperationStatus Operating
                                      setOperation Delete
                                      fbr <- launchAff $ runExceptT $ deleteBook isbn
                                      launchAff_ do
                                        r <- joinFiber fbr
                                        liftEffect $ setOperationStatus case r of
                                          Left err -> Errored $ "Hata:" <> err
                                          Right NotFound -> Errored "Kitap bulunamadı!"
                                          Right _ -> Operated

                                  [ text_ "Sil" ]
                              ]
                      ]
                  , ((result <|**> (operation $> spyWith "opr" show <<< Just)) <|> pure (spyWith "mbBookProps" show mbBookProps))
                      <#~> \mbBP -> fixed
                        [ fixed $ radioKeys <#> \name -> radioPropInput name setPropResult
                            (Map.lookup name props)
                            (Array.head =<< Map.lookup name =<< spyWith "mbBP" show mbBP)
                        , fixed $ checkboxKeys <#> \name -> checkboxPropInput name setPropResult
                            (Map.lookup name props)
                            (Map.lookup name =<< mbBP)
                        ]
                  ]
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
    -> Maybe BookPropValue
    -> Domable lock payload
  radioPropInput name setResult mbVals mbVal = Deku.do
    setLocal /\ local <- useHot $ fromMaybe "" mbVal
    let
      engPropName = turkCharsToEng name
      cls = fold (\_ s -> "form-control" <> if String.null s then " is-invalid" else "") "form-control" local
      setBoth v = setLocal v *> setResult (name /\ [ v ])
    D.fieldset (klass_ "col-sm-6") -- (klass $ result <#> String.null >>> (if _ then " text-danger" else "") >>> ("col-6" <> _))
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
    -> Maybe BookPropValues
    -> Domable lock payload
  checkboxPropInput name setResult mbOptions mbVals = Deku.do
    let
      engName = turkCharsToEng name
      valsUnique = case mbVals of
        Nothing -> []
        Just vals -> vals # Array.nub # Array.sort
      optsUnique =
        case mbOptions of
          Nothing -> []
          Just options -> options
            <#> String.lines >>> map String.trim >>> Array.filter (not <<< String.null)
            # Array.concat
            # Array.nub
            # Array.sort
            # merge2 valsUnique
    D.fieldset (klass_ "col-sm-6 row")
      [ D.legend_ [ text_ name ]
      , case optsUnique of
          [] -> infoTokat name
          _ ->
            let
              lenUnique = Array.length optsUnique
              nothArray len = Array.replicate len Nothing
              initialArray = case valsUnique of
                [] -> nothArray lenUnique
                _ -> mergeNothing optsUnique valsUnique
            in
              Deku.do
                setLocal /\ local <- useHot initialArray
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
                  [ fixed $ optsUnique # Array.mapWithIndex
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
      setChk /\ _chk_ <- useHot added
      -- setTxt /\ txt <- useState Nothing
      setCls /\ cls <- useHot if added then FormControl else LabelFor
      let
        prevLoc = withLast local <#> \{ last } -> join last
        chk = pure added <|> (local <#> isJust)
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

    merge2 :: forall a. Ord a => Array a -> Array a -> Array a
    merge2 as bs = case Array.uncons as, Array.uncons bs of
      Just { head: a, tail: as' }, Just { head: b, tail: bs' }
        | a == b -> a : merge2 as' bs'
        | a < b -> a : merge2 as' bs
        | otherwise -> b : merge2 as bs'
      Nothing, Just { head: b, tail: bs' } -> b : merge2 [] bs'
      Just { head: a, tail: as' }, Nothing -> a : merge2 as' []
      _, _ -> []

    mergeNothing :: forall a. Show a => Ord a => Array a -> Array a -> Array (Maybe a)
    mergeNothing as bs = case Array.uncons as, Array.uncons bs of
      Just { head: a, tail: as' }, Just { head: b, tail: bs' }
        | a == b -> Just a : mergeNothing as' bs'
        | a < b -> Nothing : mergeNothing as' bs
        | otherwise -> unsafeCrashWith $ "mergeNothing impossible condition as: " <> show as <> ", bs: " <> show bs
      Just { head: _, tail: as' }, Nothing -> Nothing : mergeNothing as' []
      Nothing, Just _ ->
        unsafeCrashWith $ "mergeNothing impossible pattern match as: " <> show as <> ", bs: " <> show bs
      _, _ -> []

eventTargetTo :: forall html. (EventTarget -> Maybe html) -> WebEvent.Event -> Maybe html
eventTargetTo fromTarget = WebEvent.target >=> fromTarget

infoTokat :: forall lock payload. String -> Domable lock payload
infoTokat name = infoPanel $ name <> " Toplu Katalog'dan gelen kitap bilgilerinin arasında bulunamadı"
