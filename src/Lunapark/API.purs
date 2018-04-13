module Lunapark.API where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Error.Class (class MonadThrow, class MonadError, throwError)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.State.Class (class MonadState)
import Data.Argonaut as J
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Traversable as T
import Lunapark.Affjax as LA
import Lunapark.Error as LE
import Lunapark.Types as LT
import Network.HTTP.Affjax (AJAX)

import Debug.Trace (traceAnyM)

type Config =
  { session ∷ LT.SessionId
  , baseURI ∷ String
  , capabilities ∷ Array LT.Capability
  }

type State =
  { timeouts ∷ LT.Timeouts }

type LunaparkConstraints e m a
  = MonadError LE.Error m
  ⇒ MonadAff (ajax ∷ AJAX|e) m
  ⇒ MonadAsk Config m
  ⇒ MonadState State m
  ⇒ a

rethrow
  ∷ ∀ m e eff a
  . MonadThrow e m
  ⇒ MonadAff eff m
  ⇒ Aff eff (Either e a)
  → m a
rethrow a = liftAff a >>= either throwError pure

wrapEither ∷ ∀ m a. MonadThrow LE.Error m ⇒ Either String a → m a
wrapEither = either (throwError <<< LE.unknownError) pure

init
  ∷ ∀ e m
  . MonadAff (ajax ∷ AJAX|e) m
  ⇒ String
  → LT.CapabilitiesRequest
  → m (Either LE.Error LT.CreateSessionResponse)
init uri desiredCapabilities = do
  sessObj ← liftAff $ LA.init uri $ LT.encodeCapabilitiesRequest desiredCapabilities
  pure $ sessObj >>= \a → lmap LE.unknownError (LT.decodeCreateSessionResponse a)

quit ∷ ∀ e m. LunaparkConstraints e m (m Unit)
quit = do
  r ← ask
  void $ rethrow $ LA.quit r.baseURI $ un LT.SessionId r.session

status ∷ ∀ e m. LunaparkConstraints e m (m LT.ServerStatus)
status = do
  r ← ask
  res ← rethrow $ LA.status r.baseURI
  wrapEither $ LT.decodeServerStatus res

getTimeouts ∷ ∀ e m. LunaparkConstraints e m (m LT.Timeouts)
getTimeouts = do
  r ← ask
  res ← rethrow $ LA.getTimeouts r.baseURI $ un LT.SessionId r.session
  wrapEither $ LT.decodeTimeouts res

setTimeouts ∷ ∀ e m. LunaparkConstraints e m (LT.Timeouts → m Unit)
setTimeouts timeouts = do
  r ← ask
  void $ rethrow $ LA.setTimeouts
    r.baseURI
    (un LT.SessionId r.session)
    (LT.encodeTimeouts timeouts)

go ∷ ∀ e m. LunaparkConstraints e m (String → m Unit)
go url = do
  r ← ask
  void $ rethrow $ LA.go
    r.baseURI
    (un LT.SessionId r.session)
    (LT.encodeGoRequest url)

getCurrentUrl ∷ ∀ e m. LunaparkConstraints e m (m String)
getCurrentUrl = do
  r ← ask
  res ← rethrow $ LA.getCurrentUrl r.baseURI $ un LT.SessionId r.session
  wrapEither $ J.decodeJson res

back ∷ ∀ e m. LunaparkConstraints e m (m Unit)
back = do
  r ← ask
  void $ rethrow $ LA.back r.baseURI $ un LT.SessionId r.session

forward ∷ ∀ e m. LunaparkConstraints e m (m Unit)
forward = do
  r ← ask
  void $ rethrow $ LA.forward r.baseURI $ un LT.SessionId r.session

refresh ∷ ∀ e m. LunaparkConstraints e m (m Unit)
refresh = do
  r ← ask
  void $ rethrow $ LA.refresh r.baseURI $ un LT.SessionId r.session

getTitle ∷ ∀ e m. LunaparkConstraints e m (m String)
getTitle = do
  r ← ask
  res ← rethrow $ LA.getTitle r.baseURI $ un LT.SessionId r.session
  wrapEither $ J.decodeJson res

getWindowHandle ∷ ∀ e m. LunaparkConstraints e m (m LT.WindowHandle)
getWindowHandle = do
  r ← ask
  res ← rethrow $ LA.getWindowHandle r.baseURI $ un LT.SessionId r.session
  wrapEither $ LT.decodeWindowHandle res

closeWindow ∷ ∀ e m. LunaparkConstraints e m (m Unit)
closeWindow = do
  r ← ask
  void $ rethrow $ LA.closeWindow r.baseURI $ un LT.SessionId r.session

switchToWindow ∷ ∀ e m. LunaparkConstraints e m (LT.WindowHandle → m Unit)
switchToWindow wh = do
  r ← ask
  void $ rethrow $ LA.switchToWindow
    r.baseURI
    (un LT.SessionId r.session)
    (LT.encodeSwitchToWindowRequest wh)

getWindowHandles ∷ ∀ e m. LunaparkConstraints e m (m (Array LT.WindowHandle))
getWindowHandles = do
  r ← ask
  res ← rethrow $ LA.getWindowHandles r.baseURI $ un LT.SessionId r.session
  wrapEither $ T.traverse LT.decodeWindowHandle =<< J.decodeJson res

switchToFrame ∷ ∀ e m. LunaparkConstraints e m (LT.FrameId → m Unit)
switchToFrame fid = do
  r ← ask
  void $ rethrow $ LA.switchToFrame
    r.baseURI
    (un LT.SessionId r.session)
    (LT.encodeFrameId fid)

switchToParentFrame ∷ ∀ e m. LunaparkConstraints e m (m Unit)
switchToParentFrame = do
  r ← ask
  void $ rethrow $ LA.switchToParentFrame r.baseURI $ un LT.SessionId r.session

getWindowRect ∷ ∀ e m. LunaparkConstraints e m (m LT.Rectangle)
getWindowRect = do
  r ← ask
  res ← rethrow $ LA.getWindowRect r.baseURI $ un LT.SessionId r.session
  wrapEither $ LT.decodeRectangle res

setWindowRect ∷ ∀ e m. LunaparkConstraints e m (LT.Rectangle → m Unit)
setWindowRect rectangle = do
  r ← ask
  void $ rethrow $ LA.setWindowRect
    r.baseURI
    (un LT.SessionId r.session)
    (LT.encodeRectangle rectangle)

maximizeWindow ∷ ∀ e m. LunaparkConstraints e m (m Unit)
maximizeWindow = do
  r ← ask
  void $ rethrow $ LA.maximizeWindow r.baseURI $ un LT.SessionId r.session

minimizeWindow ∷ ∀ e m. LunaparkConstraints e m (m Unit)
minimizeWindow = do
  r ← ask
  void $ rethrow $ LA.minimizeWindow r.baseURI $ un LT.SessionId r.session

fullscreenWindow ∷ ∀ e m. LunaparkConstraints e m (m Unit)
fullscreenWindow = do
  r ← ask
  void $ rethrow $ LA.fullscreenWindow r.baseURI $ un LT.SessionId r.session

findElement ∷ ∀ m e. LunaparkConstraints e m (LT.Locator → m LT.Element)
findElement loc = do
  r ← ask
  res ←
    rethrow $ LA.findElement
      r.baseURI
      (un LT.SessionId r.session)
      (LT.encodeLocator loc)
  wrapEither $ LT.decodeElement res

findElements ∷ ∀ m e. LunaparkConstraints e m (LT.Locator → m (Array LT.Element))
findElements loc = do
  r ← ask
  res ←
    rethrow $ LA.findElements
      r.baseURI
      (un LT.SessionId r.session)
      (LT.encodeLocator loc)
  wrapEither $ T.traverse LT.decodeElement =<< J.decodeJson res

findElementFromElement
  ∷ ∀ m e
  . LunaparkConstraints e m
  ( LT.Element
  → LT.Locator
  → m LT.Element
  )
findElementFromElement el loc = do
  r ← ask
  res ←
    rethrow $ LA.findElementFromElement
      r.baseURI
      (un LT.SessionId r.session)
      (un LT.Element el)
      (LT.encodeLocator loc)
  wrapEither $ LT.decodeElement res

findElementsFromElement
  ∷ ∀ m e
  . LunaparkConstraints e m
  ( LT.Element
  → LT.Locator
  → m (Array LT.Element)
  )
findElementsFromElement el loc = do
  r ← ask
  res ←
    rethrow $ LA.findElementsFromElement
      r.baseURI
      (un LT.SessionId r.session)
      (un LT.Element el)
      (LT.encodeLocator loc)
  wrapEither $ T.traverse LT.decodeElement =<< J.decodeJson res

getActiveElement ∷ ∀ m e. LunaparkConstraints e m (m LT.Element)
getActiveElement = do
  r ← ask
  res ← rethrow $ LA.getActiveElement r.baseURI $ un LT.SessionId r.session
  wrapEither $ LT.decodeElement res

isElementSelected ∷ ∀ m e. LunaparkConstraints e m (LT.Element → m Boolean)
isElementSelected el = do
  r ← ask
  res ←
    rethrow $ LA.isElementSelected
      r.baseURI
      (un LT.SessionId r.session)
      (un LT.Element el)
  wrapEither $ J.decodeJson res

getElementAttribute ∷ ∀ m e. LunaparkConstraints e m (LT.Element → String → m (Maybe String))
getElementAttribute el attrName = do
  r ← ask
  res ←
    rethrow $ LA.getElementAttribute
      r.baseURI
      (un LT.SessionId r.session)
      (un LT.Element el)
      attrName
  if J.isNull res
    then pure Nothing
    else wrapEither $ J.decodeJson res

getElementProperty ∷ ∀ m e. LunaparkConstraints e m (LT.Element → String → m J.Json)
getElementProperty el propName = do
  r ← ask
  rethrow $ LA.getElementProperty
    r.baseURI
    (un LT.SessionId r.session)
    (un LT.Element el)
    propName

getElementCSSValue ∷ ∀ m e. LunaparkConstraints e m (LT.Element → String → m String)
getElementCSSValue el cssName = do
  r ← ask
  res ←
    rethrow $ LA.getElementCSSValue
      r.baseURI
      (un LT.SessionId r.session)
      (un LT.Element el)
      cssName
  wrapEither $ J.decodeJson res

getElementText ∷ ∀ m e. LunaparkConstraints e m (LT.Element → m String)
getElementText el = do
  r ← ask
  res ←
    rethrow $ LA.getElementText
      r.baseURI
      (un LT.SessionId r.session)
      (un LT.Element el)
  wrapEither $ J.decodeJson res

getElementTagName ∷ ∀ m e. LunaparkConstraints e m (LT.Element → m String)
getElementTagName el = do
  r ← ask
  res ←
    rethrow $ LA.getElementTagName
      r.baseURI
      (un LT.SessionId r.session)
      (un LT.Element el)
  wrapEither $ J.decodeJson res

getElementRect ∷ ∀ m e. LunaparkConstraints e m (LT.Element → m LT.Rectangle)
getElementRect el = do
  r ← ask
  res ←
    rethrow $ LA.getElementRect
      r.baseURI
      (un LT.SessionId r.session)
      (un LT.Element el)
  wrapEither $ LT.decodeRectangle res

isElementEnabled ∷ ∀ m e. LunaparkConstraints e m (LT.Element → m Boolean)
isElementEnabled el = do
  r ← ask
  res ←
    rethrow $ LA.isElementEnabled
      r.baseURI
      (un LT.SessionId r.session)
      (un LT.Element el)
  wrapEither $ J.decodeJson res

elementClick ∷ ∀ m e. LunaparkConstraints e m (LT.Element → m Unit)
elementClick el = do
  r ← ask
  void $ rethrow $ LA.elementClick
    r.baseURI
    (un LT.SessionId r.session)
    (un LT.Element el)

elementClear ∷ ∀ m e. LunaparkConstraints e m (LT.Element → m Unit)
elementClear el = do
  r ← ask
  void $ rethrow $ LA.elementClear
    r.baseURI
    (un LT.SessionId r.session)
    (un LT.Element el)

elementSendKeys ∷ ∀ m e. LunaparkConstraints e m (LT.Element → String → m Unit)
elementSendKeys el txt = do
  r ← ask
  void $ rethrow $ LA.elementSendKeys
    r.baseURI
    (un LT.SessionId r.session)
    (un LT.Element el)
    (LT.encodeSendKeysRequest txt)

getPageSource ∷ ∀ m e. LunaparkConstraints e m (m String)
getPageSource = do
  r ← ask
  res ← rethrow $ LA.getPageSource r.baseURI (un LT.SessionId r.session)
  wrapEither $ J.decodeJson res

executeScript ∷ ∀ m e. LunaparkConstraints e m (LT.Script → m J.Json)
executeScript scr = do
  r ← ask
  rethrow $ LA.executeScript
    r.baseURI
    (un LT.SessionId r.session)
    (LT.encodeScript scr)

executeAsyncScript ∷ ∀ m e. LunaparkConstraints e m (LT.Script → m J.Json)
executeAsyncScript scr = do
  r ← ask
  rethrow $ LA.executeAsyncScript
    r.baseURI
    (un LT.SessionId r.session)
    (LT.encodeScript scr)

getAllCookies ∷ ∀ m e. LunaparkConstraints e m (m (Array LT.Cookie))
getAllCookies = do
  r ← ask
  res ← rethrow $ LA.getAllCookies r.baseURI $ un LT.SessionId r.session
  wrapEither $ T.traverse LT.decodeCookie =<< J.decodeJson res

getNamedCookie ∷ ∀ m e. LunaparkConstraints e m (String → m LT.Cookie)
getNamedCookie name = do
  r ← ask
  res ← rethrow $ LA.getNamedCookie r.baseURI (un LT.SessionId r.session) name
  wrapEither $ LT.decodeCookie res

addCookie ∷ ∀ m e. LunaparkConstraints e m (LT.Cookie → m Unit)
addCookie cookie = do
  r ← ask
  void $ rethrow $ LA.addCookie
    r.baseURI
    (un LT.SessionId r.session)
    (LT.encodeCookie cookie)

deleteCookie ∷ ∀ m e. LunaparkConstraints e m (String → m Unit)
deleteCookie name = do
  r ← ask
  void $ rethrow $ LA.deleteCookie
    r.baseURI
    (un LT.SessionId r.session)
    name

deleteAllCookies ∷ ∀ m e. LunaparkConstraints e m (m Unit)
deleteAllCookies = do
  r ← ask
  void $ rethrow $ LA.deleteAllCookies r.baseURI $ un LT.SessionId r.session

performActions ∷ ∀ m e. LunaparkConstraints e m (LT.ActionRequest → m Unit)
performActions req = do
  r ← ask
  void $ rethrow $ LA.performActions
    r.baseURI
    (un LT.SessionId r.session)
    (LT.encodeActionRequest req)

releaseActions ∷ ∀ m e. LunaparkConstraints e m (m Unit)
releaseActions = do
  r ← ask
  void $ rethrow $ LA.releaseActions r.baseURI $ un LT.SessionId r.session

dismissAlert ∷ ∀ m e. LunaparkConstraints e m (m Unit)
dismissAlert = do
  r ← ask
  void $ rethrow $ LA.dismissAlert r.baseURI $ un LT.SessionId r.session

acceptAlert ∷ ∀ m e. LunaparkConstraints e m (m Unit)
acceptAlert = do
  r ← ask
  void $ rethrow $ LA.acceptAlert r.baseURI $ un LT.SessionId r.session

getAlertText ∷ ∀ m e. LunaparkConstraints e m (m String)
getAlertText = do
  r ← ask
  res ← rethrow $ LA.getAlertText r.baseURI $ un LT.SessionId r.session
  wrapEither $ J.decodeJson res

sendAlertText ∷ ∀ m e. LunaparkConstraints e m (String → m Unit)
sendAlertText s = do
  r ← ask
  void $ rethrow $ LA.sendAlertText
    r.baseURI
    (un LT.SessionId r.session)
    (LT.encodeSendKeysRequest s)

takeScreenshot ∷ ∀ m e. LunaparkConstraints e m (m LT.Screenshot)
takeScreenshot = do
  r ← ask
  res ← rethrow $ LA.takeScreenshot
    r.baseURI
    (un LT.SessionId r.session)
  wrapEither $ LT.decodeScreenshot res

takeElementScreenshot ∷ ∀ m e. LunaparkConstraints e m (LT.Element → m LT.Screenshot)
takeElementScreenshot el = do
  r ← ask
  res ← rethrow $ traceAnyM =<< LA.takeElementScreenshot
    r.baseURI
    (un LT.SessionId r.session)
    (un LT.Element el)
  wrapEither $ LT.decodeScreenshot res

-- Recomended by W3C
isDisplayed ∷ ∀ m e. LunaparkConstraints e m (LT.Element → m Boolean)
isDisplayed el = do
  r ← ask
  res ← rethrow $ LA.isDisplayed
    r.baseURI
    (un LT.SessionId r.session)
    (un LT.Element el)
  wrapEither $ J.decodeJson res

-- JsonWire legacy
setWireTimeouts ∷ ∀ m e. LunaparkConstraints e m (LT.Timeouts → m Unit)
setWireTimeouts ts = do
  r ← ask
  T.for_ (LT.encodeLegacyTimeouts ts) \j →
    void $ rethrow $ LA.setTimeouts r.baseURI (un LT.SessionId r.session) j
