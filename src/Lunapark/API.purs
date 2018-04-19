module Lunapark.API where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff as Aff
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut as J
import Data.Bifunctor (lmap)
import Data.Either (Either(..), isRight)
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.StrMap as SM
import Data.Variant as V
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable as T
import Data.Tuple (Tuple(..))
import Lunapark.Endpoint as LP
import Lunapark.Error as LE
import Lunapark.Types as LT
import Network.HTTP.Affjax (AJAX)
import Node.Buffer as B
import Node.FS.Aff as FS
import Run as R
import Run.Except (EXCEPT)
import Run.Except as RE
import Unsafe.Coerce (unsafeCoerce)

--import Debug.Trace as DT

type LunaparkEffects e =
  ( ajax ∷ AJAX
  , ref ∷ Ref.REF
  , buffer ∷ B.BUFFER
  , fs ∷ FS.FS
  | e)

-- | Example
-- | ```purescript
-- | runLunapark ← init "http://localhost:4444/wd/hub" SM.empty
-- | result ← runLunapark do
-- |   setTimeouts timeouts
-- |   go "http://google.com"
-- |   findElement ...
-- | ```
init
  ∷ ∀ e m r a
  . MonadAff (LunaparkEffects e) m
  ⇒ MonadRec m
  ⇒ MonadThrow LE.Error m
  ⇒ String
  → LT.CapabilitiesRequest
  → m (Either LE.Error (Lunapark e r a → BaseRun e r a))
init uri caps = do
  res ←
    liftAff
      $ LP.post uri (LP.Session : Nil)
      $ LT.encodeCapabilitiesRequest caps

  let
    sessionResponse = do
      sessObj ← res
      lmap LE.unknownError $ LT.decodeCreateSessionResponse sessObj

  T.for sessionResponse \{ session, capabilities } → do
    timeoutsRef ←
      liftEff $ Ref.newRef
        { implicit: Milliseconds 0.0
        , pageLoad: Milliseconds 300000.0
        , script: Milliseconds 30000.0
        }

    requestMapRef ← liftEff $ Ref.newRef Map.empty

    actionsEnabled ←
      map isRight
        $ liftAff
        $ LP.post uri  (LP.InSession session : LP.Actions : Nil)
        $ LT.encodeActionRequest $ SM.fromFoldable
            [ Tuple "id" $ LT.NoSource [ V.inj (SProxy ∷ SProxy "pause") $ Milliseconds 0.0 ] ]

    let
      input =
        { timeoutsRef
        , requestMapRef
        , uri
        , session
        , capabilities
        , actionsEnabled
        }

    pure $ interpret input

interpret
  ∷ ∀ e r
  . HandleLunaparkInput
  → Lunapark e r
  ~> BaseRun e r
interpret input = runLunapark input <<< runLunaparkActions input

data LunaparkF a
  = Quit a
  | Status (LT.ServerStatus → a)
  | GetTimeouts (LT.Timeouts → a)
  | SetTimeouts LT.Timeouts a
  | GoTo String a
  | GetUrl (String → a)
  | Forward a
  | Back a
  | Refresh a
  | GetTitle (String → a)
  | GetWindowHandle (LT.WindowHandle → a)
  | GetWindowHandles (Array LT.WindowHandle → a)
  | CloseWindow a
  | SwitchToWindow LT.WindowHandle a
  | SwitchToFrame LT.FrameId a
  | SwitchToParentFrame a
  | GetWindowRectangle (LT.Rectangle → a)
  | SetWindowRectangle LT.Rectangle a
  | MaximizeWindow a
  | MinimizeWindow a
  | FullscreenWindow a
  | ExecuteScript LT.Script (J.Json → a)
  | ExecuteScriptAsync LT.Script (J.Json → a)
  | GetAllCookies (Array LT.Cookie → a)
  | GetCookie String (LT.Cookie → a)
  | DeleteCookie String a
  | DeleteAllCookies a
  | AddCookie LT.Cookie a
  | DismissAlert a
  | AcceptAlert a
  | GetAlertText (String → a)
  | SendAlertText String a
  | Screenshot String a
  | GetActiveElement (LT.Element → a)
  | FindElement LT.Locator (LT.Element → a)
  | FindElements LT.Locator (Array LT.Element → a)
  | OnElement LT.Element (ElementF a)
  | PerformActions LT.ActionRequest a
  | ReleaseActions a

data ElementF a
  = ChildElement LT.Locator (LT.Element → a)
  | ChildElements LT.Locator (Array LT.Element → a)
  | IsSelected (Boolean → a)
  | GetAttribute String (String → a)
  | GetProperty String (J.Json → a)
  | GetCss String (String → a)
  | GetText (String → a)
  | GetTagName (String → a)
  | GetRectangle (LT.Rectangle → a)
  | IsEnabled (Boolean → a)
  | ClickEl a
  | ClearEl a
  | SendKeysEl String a
  | ScreenshotEl String a
  | IsDisplayed (Boolean → a)
  | Submit a

data ActionF a
  = Click LT.Button a
  | ButtonDown LT.Button a
  | ButtonUp LT.Button a
  | DoubleClick a
  | SendKeys String a
  | MoveTo LT.PointerMove a
  | Touch (TouchF a)
  | Pause Milliseconds a

data TouchF a
  = Tap a
  | TouchDown a
  | TouchUp a
  | LongClick a
  | Flick a
  | Scroll a
  | DoubleTap a

derive instance functorLunaparkF ∷ Functor LunaparkF
derive instance functorElementF ∷ Functor ElementF
derive instance functorActionF ∷ Functor ActionF
derive instance functorTouchF ∷ Functor TouchF

type BaseRun e r = R.Run
  ( except ∷ EXCEPT LE.Error
  , aff ∷ R.AFF (LunaparkEffects e)
  , eff ∷ R.EFF (LunaparkEffects e)
  | r)

type Lunapark e r = BaseRun e (lunapark ∷ LUNAPARK, lunaparkActions ∷ LUNAPARK_ACTIONS|r)

_lunapark = SProxy ∷ SProxy "lunapark"
_lunaparkActions = SProxy ∷ SProxy "lunaparkActions"
type LUNAPARK = R.FProxy LunaparkF
type LUNAPARK_ACTIONS = R.FProxy ActionF
type WithLunapark r a = R.Run (lunapark ∷ LUNAPARK|r) a

liftLunapark ∷ ∀ a r. LunaparkF a → WithLunapark r a
liftLunapark = R.lift _lunapark

quit ∷ ∀ r. WithLunapark r Unit
quit = liftLunapark $ Quit unit

status ∷ ∀ r. WithLunapark r LT.ServerStatus
status = liftLunapark $ Status id

setTimeouts ∷ ∀ r. LT.Timeouts → WithLunapark r Unit
setTimeouts ts = liftLunapark $ SetTimeouts ts unit

getTimeouts ∷ ∀ r. WithLunapark r LT.Timeouts
getTimeouts = liftLunapark $ GetTimeouts id

go ∷ ∀ r. String → WithLunapark r Unit
go uri = liftLunapark $ GoTo uri unit

getUrl ∷ ∀ r. WithLunapark r String
getUrl = liftLunapark $ GetUrl id

forward ∷ ∀ r. WithLunapark r Unit
forward = liftLunapark $ Forward unit

back ∷ ∀ r. WithLunapark r Unit
back = liftLunapark $ Back unit

refresh ∷ ∀ r. WithLunapark r Unit
refresh = liftLunapark $ Refresh unit

getTitle ∷ ∀ r. WithLunapark r String
getTitle = liftLunapark $ GetTitle id

getWindowHandle ∷ ∀ r. WithLunapark r LT.WindowHandle
getWindowHandle = liftLunapark $ GetWindowHandle id

getWindowHandles ∷ ∀ r. WithLunapark r (Array LT.WindowHandle)
getWindowHandles = liftLunapark $ GetWindowHandles id

closeWindow ∷ ∀ r. WithLunapark r Unit
closeWindow = liftLunapark $ CloseWindow unit

switchToWindow ∷ ∀ r. LT.WindowHandle → WithLunapark r Unit
switchToWindow w = liftLunapark $ SwitchToWindow w unit

switchToFrame ∷ ∀ r. LT.FrameId → WithLunapark r Unit
switchToFrame f = liftLunapark $ SwitchToFrame f unit

switchToParentFrame ∷ ∀ r. WithLunapark r Unit
switchToParentFrame = liftLunapark $ SwitchToParentFrame unit

getWindowRectangle ∷ ∀ r. WithLunapark r LT.Rectangle
getWindowRectangle = liftLunapark $ GetWindowRectangle id

setWindowRectangle ∷ ∀ r. LT.Rectangle → WithLunapark r Unit
setWindowRectangle r = liftLunapark $ SetWindowRectangle r unit

maximizeWindow ∷ ∀ r. WithLunapark r Unit
maximizeWindow = liftLunapark $ MaximizeWindow unit

minimizeWindow ∷ ∀ r. WithLunapark r Unit
minimizeWindow = liftLunapark $ MinimizeWindow unit

fullscreenWindow ∷ ∀ r. WithLunapark r Unit
fullscreenWindow = liftLunapark $ FullscreenWindow unit

executeScript ∷ ∀ r. LT.Script → WithLunapark r J.Json
executeScript script = liftLunapark $ ExecuteScript script id

executeScriptAsync ∷ ∀ r. LT.Script → WithLunapark r J.Json
executeScriptAsync script = liftLunapark $ ExecuteScriptAsync script id

getAllCookies ∷ ∀ r. WithLunapark r (Array LT.Cookie)
getAllCookies = liftLunapark $ GetAllCookies id

getCookie ∷ ∀ r. String → WithLunapark r LT.Cookie
getCookie name = liftLunapark $ GetCookie name id

addCookie ∷ ∀ r. LT.Cookie → WithLunapark r Unit
addCookie cookie = liftLunapark $ AddCookie cookie unit

deleteCookie ∷ ∀ r. String → WithLunapark r Unit
deleteCookie name = liftLunapark $ DeleteCookie name unit

deleteAllCookies ∷ ∀ r. WithLunapark r Unit
deleteAllCookies = liftLunapark $ DeleteAllCookies unit

dismissAlert ∷ ∀ r. WithLunapark r Unit
dismissAlert = liftLunapark $ DismissAlert unit

acceptAlert ∷ ∀ r. WithLunapark r Unit
acceptAlert = liftLunapark $ AcceptAlert unit

getAlertText ∷ ∀ r. WithLunapark r String
getAlertText = liftLunapark $ GetAlertText id

sendAlertText ∷ ∀ r. String → WithLunapark r Unit
sendAlertText txt = liftLunapark $ SendAlertText txt unit

screenshot ∷ ∀ r. String → WithLunapark r Unit
screenshot fp = liftLunapark $ Screenshot fp unit

elementScreenshot ∷ ∀ r. LT.Element → String → WithLunapark r Unit
elementScreenshot el fp = liftLunapark $ OnElement el $ ScreenshotEl fp unit

findElement ∷ ∀ r. LT.Locator → WithLunapark r LT.Element
findElement l = liftLunapark $ FindElement l id

findElements ∷ ∀ r. LT.Locator → WithLunapark r (Array LT.Element)
findElements l = liftLunapark $ FindElements l id

childElement ∷ ∀ r. LT.Element → LT.Locator → WithLunapark r LT.Element
childElement el l = liftLunapark $ OnElement el $ ChildElement l id

childElements ∷ ∀ r. LT.Element → LT.Locator → WithLunapark r (Array LT.Element)
childElements el l = liftLunapark $ OnElement el $ ChildElements l id

isSelected ∷ ∀ r. LT.Element → WithLunapark r Boolean
isSelected el = liftLunapark $ OnElement el $ IsSelected id

getAttribute ∷ ∀ r. LT.Element → String → WithLunapark r String
getAttribute el name = liftLunapark $ OnElement el $ GetAttribute name id

getProperty ∷ ∀ r. LT.Element → String → WithLunapark r J.Json
getProperty el name = liftLunapark $ OnElement el $ GetProperty name id

getCss ∷ ∀ r. LT.Element → String → WithLunapark r String
getCss el name = liftLunapark $ OnElement el $ GetCss name id

getText ∷ ∀ r. LT.Element → WithLunapark r String
getText el = liftLunapark $ OnElement el $ GetText id

getTagName ∷ ∀ r. LT.Element → WithLunapark r String
getTagName el = liftLunapark $ OnElement el $ GetTagName id

getRectangle ∷ ∀ r. LT.Element → WithLunapark r LT.Rectangle
getRectangle el = liftLunapark $ OnElement el $ GetRectangle id

isEnabled ∷ ∀ r. LT.Element → WithLunapark r Boolean
isEnabled el = liftLunapark $ OnElement el $ IsEnabled id

clickElement ∷ ∀ r. LT.Element → WithLunapark r Unit
clickElement el = liftLunapark $ OnElement el $ ClickEl unit

clearElement ∷ ∀ r. LT.Element → WithLunapark r Unit
clearElement el = liftLunapark $ OnElement el $ ClearEl unit

sendKeysElement ∷ ∀ r. LT.Element → String → WithLunapark r Unit
sendKeysElement el txt = liftLunapark $ OnElement el $ SendKeysEl txt unit

isDisplayed ∷ ∀ r. LT.Element → WithLunapark r Boolean
isDisplayed el = liftLunapark $ OnElement el $ IsDisplayed id

submitElement ∷ ∀ r. LT.Element → WithLunapark r Unit
submitElement el = liftLunapark $ OnElement el $ Submit unit

performActions ∷ ∀ r. LT.ActionRequest → WithLunapark r Unit
performActions req = liftLunapark $ PerformActions req unit

releaseActions ∷ ∀ r. WithLunapark r Unit
releaseActions = liftLunapark $ ReleaseActions unit

runLunapark ∷ ∀ e r. HandleLunaparkInput → BaseRun e (lunapark ∷ LUNAPARK|r) ~> BaseRun e r
runLunapark input = do
  R.interpretRec (R.on _lunapark (handleLunapark input) R.send)

runLunaparkActions ∷ ∀ e r. HandleLunaparkInput → Lunapark e r ~> BaseRun e (lunapark ∷ LUNAPARK|r)
runLunaparkActions input =
  if input.actionsEnabled
  then interpretW3CActions input SM.empty
  else R.interpretRec (R.on _lunaparkActions (jsonWireActions input) R.send)

interpretW3CActions
  ∷ ∀ e r
  . HandleLunaparkInput
  → LT.ActionRequest
  → Lunapark e r
  ~> BaseRun e (lunapark ∷ LUNAPARK|r)
interpretW3CActions inp acc as = case R.peel as of
  Left la → case tag la of
    Left a → w3cActions acc (interpretW3CActions inp) a
    Right others → do
      performActions acc
      cont ← R.send others
      interpretW3CActions inp SM.empty cont
  Right a → pure a
  where
  tag = R.on _lunaparkActions Left Right

w3cActions
  ∷ ∀ e r a
  . LT.ActionRequest
  → ( LT.ActionRequest
    → Lunapark e r
    ~> BaseRun e (lunapark ∷ LUNAPARK|r)
    )
  → ActionF (Lunapark e r a)
  → BaseRun e (lunapark ∷ LUNAPARK|r) a
w3cActions acc loop = case _ of
  Click btn next → loop acc next
  ButtonDown btn next → loop acc next
  ButtonUp btn next → loop acc next
  DoubleClick next → loop acc next
  MoveTo move next → loop acc next
  SendKeys txt next → loop acc next
  Pause ms next → loop acc next
  Touch tch → case tch of
    Tap next → loop acc next
    TouchDown next → loop acc next
    TouchUp next → loop acc next
    LongClick next → loop acc next
    Flick next → loop acc next
    Scroll next → loop acc next
    DoubleTap next → loop acc next

type HandleLunaparkInput =
  { session ∷ LT.SessionId
  , timeoutsRef ∷ Ref.Ref LT.Timeouts
  , requestMapRef ∷ Ref.Ref (Map.Map String Boolean)
  , uri ∷ String
  , capabilities ∷ Array LT.Capability
  , actionsEnabled ∷ Boolean
  }

jsonWireActions ∷ ∀ e r. HandleLunaparkInput → ActionF ~> BaseRun e (lunapark ∷ LUNAPARK|r)
jsonWireActions inp = case _ of
  Click btn next → do
    _ ← post (LP.Click : Nil) (LT.encodeButton btn)
    pure next
  ButtonDown btn next → do
    _ ← post (LP.ButtonDown : Nil) (LT.encodeButton btn)
    pure next
  ButtonUp btn next → do
    _ ← post (LP.ButtonUp : Nil) (LT.encodeButton btn)
    pure next
  DoubleClick next → do
    _ ← post_ (LP.DoubleClick : Nil)
    pure next
  SendKeys txt next → do
    _ ← post (LP.Keys : Nil) (LT.encodeSendKeysRequest txt)
    pure next
  MoveTo move next → do
    element ← case move.origin of
      LT.FromViewport → map Just $ findElement $ LT.ByTagName "body"
      LT.FromPointer → pure Nothing
      LT.FromElement el → pure $ Just el
    let req = { xoffset: move.x, yoffset: move.y, element }
    _ ← post (LP.MoveTo : Nil) (LT.encodeMoveToRequest req)
    pure next
  Pause ms next → do
    R.liftAff $ Aff.delay ms
    pure next
  Touch tch → case tch of
    Tap next → do
      _ ← post_ (LP.Touch : LP.Click : Nil)
      pure next
    TouchDown next → do
      _ ← post_ (LP.Touch : LP.Down : Nil)
      pure next
    TouchUp next → do
      _ ← post_ (LP.Touch : LP.Up : Nil)
      pure next
    LongClick next → do
      _ ← post_ (LP.Touch : LP.LongClick : Nil)
      pure next
    Flick next → do
      _ ← post_ (LP.Touch : LP.Flick : Nil)
      pure next
    Scroll next → do
      _ ← post_ (LP.Touch : LP.Scroll : Nil)
      pure next
    DoubleTap next → do
      _ ← post_ (LP.Touch : LP.DoubleClick : Nil)
      pure next
  where
  post a b = liftAndRethrow $ LP.post inp.uri (inSession : a) b
  post_ a = liftAndRethrow $ LP.post_ inp.uri (inSession : a)

  inSession ∷ LP.EndpointPart
  inSession = LP.InSession inp.session

handleLunapark ∷ ∀ e r. HandleLunaparkInput → LunaparkF ~> BaseRun e r
handleLunapark inp = case _ of
  Quit next → do
    _ ← delete $ inSession : Nil
    pure next
  Status cont → do
    res ← get $ LP.Status : Nil
    ss ← throwLeft $ LT.decodeServerStatus res
    pure $ cont ss
  GetTimeouts cont → do
    res ← R.liftEff $ Ref.readRef inp.timeoutsRef
    pure $ cont res
  SetTimeouts ts next → do
    R.liftEff $ Ref.writeRef inp.timeoutsRef ts
    withFallback "set timeouts"
      { w3c: void $ post (inSession : LP.Timeouts : Nil) (LT.encodeTimeouts ts)
      , jsonWire: do
           T.for_ (LT.encodeLegacyTimeouts ts) \j →
             void $ post (inSession : LP.Timeouts : Nil) j
      }
    pure next
  GoTo uri next → do
    _ ← post (inSession : LP.Url : Nil) $ LT.encodeGoRequest uri
    pure next
  GetUrl cont → do
    res ← get $ inSession : LP.Url : Nil
    map cont $ throwLeft $ J.decodeJson res
  Back next → do
    _ ← post_ (inSession : LP.Back : Nil)
    pure next
  Forward next → do
    _ ← post_ (inSession : LP.Forward : Nil)
    pure next
  Refresh next → do
    _ ← post_ (inSession : LP.Refresh : Nil)
    pure next
  GetTitle cont → do
    res ← get (inSession : LP.Title : Nil)
    map cont $ throwLeft $ J.decodeJson res
  GetWindowHandle cont → do
    res ← withFallback "get window handle"
      { w3c: get (inSession : LP.Window : Nil)
      , jsonWire: get (inSession : LP.WindowHandle : Nil)
      }
    map cont $ throwLeft $ LT.decodeWindowHandle res
  GetWindowHandles cont → do
    res ← withFallback "get window handles"
      { w3c: get (inSession : LP.Window : LP.Handles : Nil)
      , jsonWire: get (inSession : LP.WindowHandles : Nil)
      }
    map cont $ throwLeft $ T.traverse LT.decodeWindowHandle =<< J.decodeJson res
  CloseWindow next → do
    _ ← delete (inSession : LP.Window : Nil)
    pure next
  SwitchToWindow w next → do
    _ ← post (inSession : LP.Window : Nil) (LT.encodeSwitchToWindowRequest w)
    pure next
  SwitchToFrame fid next → do
    _ ← post (inSession : LP.Frame : Nil) (LT.encodeFrameId fid)
    pure next
  SwitchToParentFrame next → do
    _ ← post_ (inSession : LP.Frame : LP.Parent : Nil)
    pure next
  GetWindowRectangle cont → do
    res ← withFallback "get window rectangle"
      { w3c: do
           res ← get (inSession : LP.Window : LP.Rect : Nil)
           throwLeft $ LT.decodeRectangle res
      , jsonWire: do
           position ← get (inSession : LP.Window : LP.Position : Nil)
           size ← get (inSession : LP.Window : LP.Size : Nil)
           throwLeft $ LT.decodeRectangleLegacy { position, size }
      }
    pure $ cont res
  SetWindowRectangle r next → do
    withFallback "set window rectangle"
      { w3c: do
           void $ post (inSession : LP.Window : LP.Rect : Nil) (LT.encodeRectangle r)
      , jsonWire: do
           let js = LT.encodeRectangleLegacy r
           _ ← post (inSession : LP.Window : LP.Size : Nil) js.size
           void $ post (inSession : LP.Window : LP.Position : Nil) js.position
      }
    pure next
  MaximizeWindow next → do
    _ ← post_ (inSession : LP.Window : LP.Maximize : Nil)
    pure next
  MinimizeWindow next → do
    _ ← post_ (inSession : LP.Window : LP.Minimize : Nil)
    pure next
  FullscreenWindow next → do
    _ ← post_ (inSession : LP.Window : LP.Fullscreen : Nil)
    pure next
  ExecuteScript script cont → do
    map cont $ withFallback "execute script"
      { w3c: post (inSession : LP.Execute : LP.Sync : Nil) (LT.encodeScript script)
      , jsonWire: post (inSession : LP.Execute : Nil) (LT.encodeScript script)
      }
  ExecuteScriptAsync script cont → do
    map cont $ withFallback "execute script async"
      { w3c: post (inSession : LP.Execute : LP.Async : Nil) (LT.encodeScript script)
      , jsonWire: post (inSession : LP.ExecuteAsync : Nil) (LT.encodeScript script)
      }
  GetAllCookies cont → do
    res ← get (inSession : LP.Cookies : Nil)
    map cont $ throwLeft $ T.traverse LT.decodeCookie =<< J.decodeJson res
  GetCookie name cont → do
    res ← get (inSession : LP.Cookie name : Nil)
    map cont $ throwLeft $ LT.decodeCookie res
  DeleteAllCookies next → do
    _ ← delete (inSession : LP.Cookies : Nil)
    pure next
  DeleteCookie name next → do
    _ ← delete (inSession : LP.Cookie name : Nil)
    pure next
  AddCookie cookie next → do
    _ ← post (inSession : LP.Cookies : Nil) (LT.encodeCookie cookie)
    pure next
  DismissAlert next → do
    _ ← withFallback "dismiss alert"
      { w3c: post_ (inSession : LP.Alert : LP.Dismiss : Nil)
      , jsonWire: post_ (inSession : LP.DismissAlert : Nil)
      }
    pure next
  AcceptAlert next → do
    _ ← withFallback "accept alert"
      { w3c: post_ (inSession : LP.Alert : LP.Accept : Nil)
      , jsonWire: post_ (inSession : LP.AcceptAlert : Nil)
      }
    pure next
  GetAlertText cont → do
    res ← withFallback "get alert text"
      { w3c: get (inSession : LP.Alert : LP.Text : Nil)
      , jsonWire: get (inSession : LP.AlertText : Nil)
      }
    map cont $ throwLeft $ J.decodeJson res
  SendAlertText str next → do
    _ ← withFallback "send alert text"
      { w3c: post (inSession : LP.Alert : LP.Text : Nil) (LT.encodeSendKeysRequest str)
      , jsonWire: post (inSession : LP.AlertText : Nil) (LT.encodeSendKeysRequest str)
      }
    pure next
  Screenshot fp next → do
    res ← get (inSession : LP.Screenshot : Nil)
    screenshotPack ← throwLeft $ LT.decodeScreenshot res
    buffer ← R.liftEff $ B.fromString screenshotPack.content screenshotPack.encoding
    R.liftAff $ FS.writeFile fp buffer
    pure next
  FindElement loc cont → do
    res ← post (inSession : LP.Element : Nil) (LT.encodeLocator loc)
    map cont $ throwLeft $ LT.decodeElement res
  FindElements loc cont → do
    res ← post (inSession : LP.Elements : Nil) (LT.encodeLocator loc)
    map cont $ throwLeft $ T.traverse LT.decodeElement =<< J.decodeJson res
  GetActiveElement cont → do
    res ← get (inSession : LP.Element : LP.Active : Nil)
    map cont $ throwLeft $ LT.decodeElement res
  PerformActions req next → do
    when inp.actionsEnabled
      $ void $ post
        (inSession : LP.Actions : Nil)
        (LT.encodeActionRequest req)
    pure next
  ReleaseActions next → do
    when inp.actionsEnabled $ void $ delete (inSession : LP.Actions : Nil)
    pure next
  OnElement el elF →
    let inElement = LP.InElement el
    in case elF of
      ChildElement loc cont → do
        res ← post (inSession : inElement : LP.Element : Nil) (LT.encodeLocator loc)
        map cont $ throwLeft $ LT.decodeElement res
      ChildElements loc cont → do
        res ← post (inSession : inElement : LP.Elements : Nil) (LT.encodeLocator loc)
        map cont $ throwLeft $ T.traverse LT.decodeElement =<< J.decodeJson res
      ScreenshotEl fp next → do
        res ← get (inSession : inElement : LP.Screenshot : Nil)
        screenshotPack ← throwLeft $ LT.decodeScreenshot res
        buffer ← R.liftEff $ B.fromString screenshotPack.content screenshotPack.encoding
        R.liftAff $ FS.writeFile fp buffer
        pure next
      IsSelected cont → do
        res ← get (inSession : inElement : LP.Selected : Nil)
        map cont $ throwLeft $ J.decodeJson res
      GetAttribute attr cont → do
        res ← get (inSession : inElement : LP.Attribute attr : Nil)
        map cont $ throwLeft $ J.decodeJson res
      GetProperty prop cont → do
        map cont $ get (inSession : inElement : LP.Property prop : Nil)
      GetCss css cont → do
        res ← get (inSession : inElement : LP.CssValue css : Nil)
        map cont $ throwLeft $ J.decodeJson res
      GetText cont → do
        res ← get (inSession : inElement : LP.Text : Nil)
        map cont $ throwLeft $ J.decodeJson res
      GetTagName cont → do
        res ← get (inSession : inElement : LP.Name : Nil)
        map cont $ throwLeft $ J.decodeJson res
      GetRectangle cont →
        map cont $ withFallback "get element rectangle"
          { w3c: do
             res ← get (inSession : inElement : LP.Rect : Nil)
             throwLeft $ LT.decodeRectangle res
          , jsonWire: do
             position ← get (inSession : inElement : LP.Position : Nil)
             size ← get (inSession : inElement : LP.Size : Nil)
             throwLeft $ LT.decodeRectangleLegacy { position, size }
          }
      IsEnabled cont → do
        res ← get (inSession : inElement : LP.Enabled : Nil)
        map cont $ throwLeft $ J.decodeJson res
      ClickEl next → do
        _ ← post_ (inSession : inElement : LP.Click : Nil)
        pure next
      ClearEl next → do
        _ ← post_ (inSession : inElement : LP.Clear : Nil)
        pure next
      SendKeysEl txt next → do
        _ ← post (inSession : inElement : LP.Value : Nil) (LT.encodeSendKeysRequest txt)
        pure next
      IsDisplayed cont → do
        res ← withFallback "is element displayed"
          { w3c: get (inSession : inElement : LP.Displayed : Nil)
          , jsonWire: do
               let script =
                     { script: """var el = arguments[0]; return el.offsetHeight > 0 && el.offsetWidth > 0"""
                     , args: [ LT.encodeElement el ]
                     }
               handleLunapark inp $ ExecuteScript script id
          }
        map cont $ throwLeft $ J.decodeJson res
      Submit next → do
        _ ← post_ (inSession : inElement : LP.Submit : Nil)
        pure next

  where
  delete a = liftAndRethrow $ LP.delete inp.uri a
  post a b = liftAndRethrow $ LP.post inp.uri a b
  get a = liftAndRethrow $ LP.get inp.uri a
  post_ a = liftAndRethrow $ LP.post_ inp.uri a

  withFallback ∷ ∀ a. String → { w3c ∷ BaseRun e r a, jsonWire ∷ BaseRun e r a } → BaseRun e r a
  withFallback key { w3c: try, jsonWire: fallback } = do
    mp ← R.liftEff $ Ref.readRef inp.requestMapRef
    case Map.lookup key mp of
      Just true → try
      Just false → fallback
      Nothing →
        let
          try' = do
            a ← try
            R.liftEff $ Ref.modifyRef inp.requestMapRef (Map.insert key true)
            pure a
          fallback' = do
            a ← fallback
            R.liftEff $ Ref.modifyRef inp.requestMapRef (Map.insert key false)
            pure a
        in catch try' \_ → fallback'

  inSession ∷ LP.EndpointPart
  inSession = LP.InSession inp.session

liftAndRethrow ∷ ∀ e r ω. Aff (LunaparkEffects e) (Either LE.Error ω) → BaseRun e r ω
liftAndRethrow a = do
  res ← R.liftAff a
  RE.rethrow res

throwLeft ∷ ∀ e r. Either String ~> BaseRun e r
throwLeft = RE.rethrow <<< lmap LE.unknownError

-- Safe, since we actually want handler and result have same rows not, remove except
catch ∷ ∀ e r a. R.Run (except ∷ EXCEPT e|r) a → (e → R.Run (except ∷ EXCEPT e|r) a) → R.Run (except ∷ EXCEPT e|r) a
catch = unsafeCoerce $ flip RE.catch
