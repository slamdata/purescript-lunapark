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

getWindowHandle ∷ ∀ e. URI → SessionId → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
getWindowHandle uri sessId =
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
