module Lunapark.Affjax where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Argonaut (Json)
import Data.Argonaut as J
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as N
import Network.HTTP.StatusCode (StatusCode(..))
import Lunapark.Error as LE

-- | This two type synonims are used only here
-- | this is the lowest level of bindings this lib provides
type URI = String
type SessionId = String
type ElementId = String
type WindowHandle = String

handleAPIError ∷ N.AffjaxResponse Json → Either LE.Error Json
handleAPIError r = case r.status of
  StatusCode 200 → lmap LE.unknownError do
    obj ← J.decodeJson r.response
    obj J..? "value"
  code → Left $ LE.fromJson r.response

init ∷ ∀ e. URI → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
init uri obj =
  map handleAPIError $ N.post (uri <> "/session") obj

quit ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
quit uri sessId =
  map handleAPIError $ N.delete (uri <> "/session/" <> sessId)

status ∷ ∀ e. URI → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
status uri =
  map handleAPIError $ N.get (uri <> "/status")

getTimeouts ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getTimeouts uri sessId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/timeouts")

setTimeouts ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
setTimeouts uri sessId timeouts =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/timeouts") timeouts

go ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
go uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/url") obj

getCurrentUrl ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getCurrentUrl uri sessId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/url")

back ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
back uri sessId =
  map handleAPIError $ N.post' (uri <> "/session/" <> sessId <> "/back") (Nothing ∷ Maybe Unit)

forward ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
forward uri sessId =
  map handleAPIError $ N.post' (uri <> "/session/" <> sessId <> "/forward") (Nothing ∷ Maybe Unit)

refresh ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
refresh uri sessId =
  map handleAPIError $ N.post' (uri <> "/session/" <> sessId <> "/refresh") (Nothing ∷ Maybe Unit)

getTitle ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getTitle uri sessId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/title")

getWindow ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getWindow uri sessId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/window")

closeWindow ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
closeWindow uri sessId =
  map handleAPIError $ N.delete (uri <> "/session/" <> sessId)

switchToWindow ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
switchToWindow uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/window") obj

getWindowHandles ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getWindowHandles uri sessId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/window/handles")

switchToFrame ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
switchToFrame uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/frame") obj

switchToParentFrame ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
switchToParentFrame uri sessId =
  map handleAPIError $ N.post' (uri <> "/session/" <> sessId <> "/frame/parent") (Nothing ∷ Maybe Unit)

getWindowRect ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getWindowRect uri sessId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/window/rect")

setWindowRect ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
setWindowRect uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/window/rect") obj

maximizeWindow ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
maximizeWindow uri sessId =
  map handleAPIError $ N.post' (uri <> "/session/" <> sessId <> "/window/maximize") (Nothing ∷ Maybe Unit)

minimizeWindow ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
minimizeWindow uri sessId =
  map handleAPIError $ N.post' (uri <> "/session/" <> sessId <> "/window/minimize") (Nothing ∷ Maybe Unit)

fullscreenWindow ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
fullscreenWindow uri sessId =
  map handleAPIError $ N.post' (uri <> "/session/" <> sessId <> "/window/fullscreen") (Nothing ∷ Maybe Unit)

findElement ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
findElement uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/element") obj

findElements ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
findElements uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/elements") obj

findElementFromElement ∷ ∀ e. URI → SessionId → ElementId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
findElementFromElement uri sessId elId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/element/" <> elId <> "/element") obj

findElementsFromElement ∷ ∀ e. URI → SessionId → ElementId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
findElementsFromElement uri sessId elId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/element/" <> elId <> "/elements") obj

getActiveElement ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getActiveElement uri sessId = map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/element/active")

isElementSelected ∷ ∀ e. URI → SessionId → ElementId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
isElementSelected uri sessId elId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/element/" <> elId <> "/selected")

getElementAttribute ∷ ∀ e. URI → SessionId → ElementId → String → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getElementAttribute uri sessId elId attrName =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/element/" <> elId <> "/attribute/" <> attrName)

getElementProperty ∷ ∀ e. URI → SessionId → ElementId → String → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getElementProperty uri sessId elId propName =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/element/" <> elId <> "/property/" <> propName)

getElementCSSValue ∷ ∀ e. URI → SessionId → ElementId → String → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getElementCSSValue uri sessId elId cssName =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/element/" <> elId <> "/css/" <> cssName)

getElementText ∷ ∀ e. URI → SessionId → ElementId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getElementText uri sessId elId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/element/" <> elId <> "/text")

getElementTagName ∷ ∀ e. URI → SessionId → ElementId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getElementTagName uri sessId elId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/element/" <> elId <> "/name")

getElementRect ∷ ∀ e. URI → SessionId → ElementId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getElementRect uri sessId elId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/element/" <> elId <> "/rect")

isElementEnabled ∷ ∀ e. URI → SessionId → ElementId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
isElementEnabled uri sessId elId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/element/" <> elId <> "/enabled")

elementClick ∷ ∀ e. URI → SessionId → ElementId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
elementClick uri sessId elId =
  map handleAPIError $ N.post' (uri <> "/session/" <> sessId <> "/element/" <> elId <> "/click") (Nothing ∷ Maybe Unit)

elementClear ∷ ∀ e. URI → SessionId → ElementId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
elementClear uri sessId elId =
  map handleAPIError $ N.post' (uri <> "/session/" <> sessId <> "/element/" <> elId <> "/clear") (Nothing ∷ Maybe Unit)

elementSendKeys ∷ ∀ e. URI → SessionId → ElementId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
elementSendKeys uri sessId elId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/element/" <> elId <> "/value") obj

getPageSource ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getPageSource uri sessId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/source")

executeScript ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
executeScript uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/execute/sync") obj

executeAsyncScript ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
executeAsyncScript uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/execute/async") obj

getAllCookies ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getAllCookies uri sessId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/cookie")

getNamedCookie ∷ ∀ e. URI → SessionId → String → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getNamedCookie uri sessId name =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/cookie/" <> name)

addCookie ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
addCookie uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/cookie") obj

deleteCookie ∷ ∀ e. URI → SessionId → String → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
deleteCookie uri sessId name =
  map handleAPIError $ N.delete (uri <> "/session/" <> sessId <> "/cookie/" <> name)

deleteAllCookies ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
deleteAllCookies uri sessId =
  map handleAPIError $ N.delete (uri <> "/session/" <> sessId <> "/cookie")

performActions ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
performActions uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/actions") obj

releaseActions ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
releaseActions uri sessId =
  map handleAPIError $ N.delete (uri <> "/session/" <> sessId <> "/actions")

dismissAlert ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
dismissAlert uri sessId =
  map handleAPIError $ N.post' (uri <> "/session/" <> sessId <> "/alert/dismiss") (Nothing ∷ Maybe Unit)

acceptAlert ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
acceptAlert uri sessId =
  map handleAPIError $ N.post' (uri <> "/session/" <> sessId <> "/alert/accept") (Nothing ∷ Maybe Unit)

getAlertText ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getAlertText uri sessId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/alert/text")

sendAlertText ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
sendAlertText uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/alert/text") obj

takeScreenshot ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
takeScreenshot uri sessId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/screenshot")

takeElementScreenshot ∷ ∀ e. URI → SessionId → ElementId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
takeElementScreenshot uri sessId elId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/element/" <> elId <> "/screenshot")

-- Recomended by W3C
isDisplayed ∷ ∀ e. URI → SessionId → ElementId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
isDisplayed uri sessId elId = do
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/element/" <> elId <> "/displayed")

-- JsonWire legacy, note this wouldn't envolve thing like local storage or geolocation
-- because you always could do it via async script _OR_ you couldn't do it via W3C
getSession ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getSession uri sessId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId)

getSessions ∷ ∀ e. URI → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getSessions uri =
  map handleAPIError $ N.get (uri <> "/sessions")

setAsyncScriptTimeout ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
setAsyncScriptTimeout uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/timeouts/async_script") obj

getWindowHandle ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getWindowHandle uri sessId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/window_handle")

getWindowHandlesLegacy ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getWindowHandlesLegacy uri sessId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/window_handles")

execute ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
execute uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/execute") obj

executeAsync ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
executeAsync uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/execute_async") obj

setWindowSize ∷ ∀ e. URI → SessionId → WindowHandle → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
setWindowSize uri sessId win obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/window/" <> win <> "/size") obj

getWindowSize ∷ ∀ e. URI → SessionId → WindowHandle → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getWindowSize uri sessId win =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/window/" <> win <> "/size")

setWindowPosition ∷ ∀ e. URI → SessionId → WindowHandle → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
setWindowPosition uri sessId win obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/window/" <> win <> "/position") obj

getWindowPosition ∷ ∀ e. URI → SessionId → WindowHandle → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getWindowPosition uri sessId win =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/window/" <> win <> "/position")

elementSubmit ∷ ∀ e. URI → SessionId → ElementId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
elementSubmit uri sessId elId =
  map handleAPIError $ N.post' (uri <> "/session/" <> sessId <> "/element/" <> elId <> "/submit") (Nothing ∷ Maybe Unit)

sendKeys ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
sendKeys uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/keys") obj

getElementLocation ∷ ∀ e. URI → SessionId → ElementId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getElementLocation uri sessId elId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/element/" <> elId <> "/location")

getElementSize ∷ ∀ e. URI → SessionId → ElementId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getElementSize uri sessId elId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/element/" <> elId <> "/size")

getAlertTextLegacy ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getAlertTextLegacy uri sessId =
  map handleAPIError $ N.get (uri <> "/session/" <> sessId <> "/alert_text")

setAlertText ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
setAlertText uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/alert_text") obj

acceptAlertLegacy ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
acceptAlertLegacy uri sessId =
  map handleAPIError $ N.post' (uri <> "/session/" <> sessId <> "/accept_alert") (Nothing ∷ Maybe Unit)

dismissAlertLegacy ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
dismissAlertLegacy uri sessId =
  map handleAPIError $ N.post' (uri <> "/session/" <> sessId <> "/dismiss_alert") (Nothing ∷ Maybe Unit)

moveTo ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
moveTo uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/moveto") obj

sendClick ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
sendClick uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/click") obj

sendButtonDown ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
sendButtonDown uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/buttondown") obj

sendButtonUp ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
sendButtonUp uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/buttonup") obj

sendDoubleClick ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
sendDoubleClick uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/doubleclick") obj

sendTouch ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
sendTouch uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/touch/click") obj

sendTouchDown ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
sendTouchDown uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/touch/down") obj

sendTouchUp ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
sendTouchUp uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/touch/up") obj

sendTouchMove ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
sendTouchMove uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/touch/move") obj

sendTouchScroll ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
sendTouchScroll uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/touch/scroll") obj

sendTouchDoubleClick ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
sendTouchDoubleClick uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/touch/doubleclick") obj

sendTouchLongClick ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
sendTouchLongClick uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/touch/longclick") obj

sendTouchFlick ∷ ∀ e. URI → SessionId → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
sendTouchFlick uri sessId obj =
  map handleAPIError $ N.post (uri <> "/session/" <> sessId <> "/touch/flick") obj
