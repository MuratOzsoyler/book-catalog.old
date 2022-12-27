module Frontend.UI where

import Prelude

import Common.BookProps (BookProps)
import Data.Array (foldl, notElem)
import Data.Filterable (filter)
import Data.Foldable (any)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust)
import Data.Show.Generic (genericShow)
import Data.String (fromCodePointArray, length, toCodePointArray, toLower)
import Data.String as String
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (!:=))
import Deku.Attributes (klass_)
import Deku.Control (blank, text_, (<#~>))
import Deku.Core (Domable)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useHot)
import Deku.Listeners (click, textInput_)
import Effect.Aff (joinFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Frontend.BookProps (collectBookProperties)
import Frontend.BookPropsInput (bookPropsInputScene)
import Frontend.UI.UIUtils (disabled, errorPanel, infoPanel, sameIdAndName, value)
import Network.RemoteData (RemoteData(..), fromEither)
import Partial.Unsafe (unsafePartial)
import QualifiedDo.Alt as Alt

data ClickStatus = NotClicked | Clicked (RemoteData String BookProps)

data ISBNStatus
  = NotEntered
  | HasInvalidChar
  | LengthError
  | ChecksumError
  | Valid String

derive instance Generic ISBNStatus _

instance Show ISBNStatus where
  show i = genericShow i

bookPropsInputPage :: forall lock payload. Domable lock payload
bookPropsInputPage = Deku.do
  setISBN /\ iSBN <- useHot NotEntered
  setRemoteData /\ remoteData <- useHot NotClicked

  D.div (klass_ "form row")
    [ D.fieldset (klass_ "offset-3 col-6")
        [ D.div (klass_ "input-group")
            [ D.input
                Alt.do
                  D.Xtype !:= "text"
                  klass_ "form-control"
                  D.Placeholder !:= "ISBN" -- Aranacak kitabın ISBN numarasını giriniz"
                  sameIdAndName "isbn"

                  D.Maxlength !:= "13"
                  D.Required !:= "required"
                  D.Pattern !:= """^\d*$"""
                  value $ iSBN
                    # filter
                        ( case _ of
                            NotEntered -> true
                            Valid _ -> true
                            _ -> false
                        )
                    <#> case _ of
                      Valid s -> s
                      _ -> ""
                  textInput_ \_v -> do
                    let v = toLower _v
                    Console.log $ "isbn: \"" <> v <> "\""
                    setISBN case length v of
                      0 -> NotEntered
                      10 ->
                        let
                          v9 = String.take 9 v
                          vx = String.drop 9 v
                        in
                          if notNumeric v9 || notNumeric vx && vx /= "x" then HasInvalidChar
                          else if check10 v then Valid v
                          else ChecksumError
                      13 ->
                        if notNumeric v then HasInvalidChar
                        else if check13 v then Valid v
                        else ChecksumError
                      _ -> LengthError
                []
            , D.button
                Alt.do
                  disabled $ not <<< isValidISBN <$> iSBN
                  klass_ "btn btn-primary btn-outline-secondary"
                  click $ iSBN <#> \_isbn -> cb $ const case _isbn of
                    Valid isbn -> do
                      setRemoteData $ Clicked NotAsked
                      fbr <- launchAff $ collectBookProperties isbn
                      setRemoteData $ Clicked Loading
                      launchAff_ do
                        props <- joinFiber fbr
                        liftEffect $ setRemoteData $ Clicked $ fromEither props
                    _ -> pure unit

                [ text_ "Ara" ]
            ]
        ]
    , ((remoteData <#> \r i -> r /\ i) <*> iSBN) <#~> case _ of
        _ /\ NotEntered -> errorPanel "ISBN giriniz"
        _ /\ HasInvalidChar -> errorPanel
          """Hatalı karakterler girilmiş.\n
(Sadece 10 haneli ISBN'lerin en son hanesi 'x' ya da 'X' ile bitebilir.
Bunun dışında rakamdan başkası kullanılamaz.)"""
        _ /\ LengthError -> errorPanel """ISBN ya 10 ya da 13 haneli olmalıdır"""
        _ /\ ChecksumError -> errorPanel "ISBN hatalı. Girişinizi kontrol ediniz. Yanlış giriş yapmadıysanız bu bir ISBN olamaz."
        NotClicked /\ Valid _ -> blank
        Clicked NotAsked /\ _ -> infoPanel "Sorgulanıyor"
        Clicked Loading /\ _ -> infoPanel "İndiriyor"
        Clicked (Failure err) /\ _ -> errorPanel $ "ISBN sorgulanırken hata oluştu: " <> err
        Clicked (Success props) /\ Valid isbn -> Deku.do
          D.div (D.Id !:= "result")
            [ bookPropsInputScene props isbn ]
    ]
  where
  isValidISBN = case _ of
    Valid _ -> true
    _ -> false
  digits = toCodePointArray "0123456789"
  notNumeric = any (_ `notElem` digits) <<< toCodePointArray
  check10 = toCodePointArray
    >>> flip foldl (Tuple 0 10)
      ( \(Tuple r i) cp ->
          let
            ch = fromCodePointArray [ cp ]
            dgt = unsafePartial $ fromJust $ case ch of
              "x" -> Just 10
              _ -> Int.fromString ch
            prod = dgt * i
            sm = r + prod
          in
            Tuple sm $ i - 1
      )
    >>> fst
    >>> (_ `div` 11)
    >>> (_ == 0)
  check13 isbn =
    let
      isbn12 = String.take 12 isbn
      isbn13 = unsafePartial $ fromJust $ Int.fromString $ String.drop 12 isbn
    in
      toCodePointArray isbn12 #
        flip foldl (Tuple 0 1)
          ( \(Tuple r n) cp ->
              let
                dgt = unsafePartial $ fromJust $ Int.fromString $ fromCodePointArray [ cp ]
                prod = dgt * n
                sm = r + prod
              in
                Tuple sm $ (n + 2) `mod` 4
          ) >>> fst >>> (_ `mod` 10) >>> (\chkd -> if chkd == 0 then 0 else 10 - chkd) >>> (_ == isbn13)
