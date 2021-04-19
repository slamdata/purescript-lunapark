module Lunapark.LunaparkF where

import Prelude

import Data.Argonaut.Core as J
import Lunapark.Types as LT
import Run (Run)
import Run as R
import Type.Proxy (Proxy(..))

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

derive instance functorLunaparkF ∷ Functor LunaparkF
derive instance functorElementF ∷ Functor ElementF

_lunapark = Proxy ∷ Proxy "lunapark"
type LUNAPARK r = ( lunapark ∷ LunaparkF | r )

liftLunapark ∷ ∀ a r. LunaparkF a → Run (LUNAPARK r) a
liftLunapark = R.lift _lunapark

quit ∷ ∀ r. Run (LUNAPARK r) Unit
quit = liftLunapark $ Quit unit

status ∷ ∀ r. Run (LUNAPARK r) LT.ServerStatus
status = liftLunapark $ Status identity

setTimeouts ∷ ∀ r. LT.Timeouts → Run (LUNAPARK r) Unit
setTimeouts ts = liftLunapark $ SetTimeouts ts unit

getTimeouts ∷ ∀ r. Run (LUNAPARK r) LT.Timeouts
getTimeouts = liftLunapark $ GetTimeouts identity

go ∷ ∀ r. String → Run (LUNAPARK r) Unit
go uri = liftLunapark $ GoTo uri unit

getUrl ∷ ∀ r. Run (LUNAPARK r) String
getUrl = liftLunapark $ GetUrl identity

forward ∷ ∀ r. Run (LUNAPARK r) Unit
forward = liftLunapark $ Forward unit

back ∷ ∀ r. Run (LUNAPARK r) Unit
back = liftLunapark $ Back unit

refresh ∷ ∀ r. Run (LUNAPARK r) Unit
refresh = liftLunapark $ Refresh unit

getTitle ∷ ∀ r. Run (LUNAPARK r) String
getTitle = liftLunapark $ GetTitle identity

getWindowHandle ∷ ∀ r. Run (LUNAPARK r) LT.WindowHandle
getWindowHandle = liftLunapark $ GetWindowHandle identity

getWindowHandles ∷ ∀ r. Run (LUNAPARK r) (Array LT.WindowHandle)
getWindowHandles = liftLunapark $ GetWindowHandles identity

closeWindow ∷ ∀ r. Run (LUNAPARK r) Unit
closeWindow = liftLunapark $ CloseWindow unit

switchToWindow ∷ ∀ r. LT.WindowHandle → Run (LUNAPARK r) Unit
switchToWindow w = liftLunapark $ SwitchToWindow w unit

switchToFrame ∷ ∀ r. LT.FrameId → Run (LUNAPARK r) Unit
switchToFrame f = liftLunapark $ SwitchToFrame f unit

switchToParentFrame ∷ ∀ r. Run (LUNAPARK r) Unit
switchToParentFrame = liftLunapark $ SwitchToParentFrame unit

getWindowRectangle ∷ ∀ r. Run (LUNAPARK r) LT.Rectangle
getWindowRectangle = liftLunapark $ GetWindowRectangle identity

setWindowRectangle ∷ ∀ r. LT.Rectangle → Run (LUNAPARK r) Unit
setWindowRectangle r = liftLunapark $ SetWindowRectangle r unit

maximizeWindow ∷ ∀ r. Run (LUNAPARK r) Unit
maximizeWindow = liftLunapark $ MaximizeWindow unit

minimizeWindow ∷ ∀ r. Run (LUNAPARK r) Unit
minimizeWindow = liftLunapark $ MinimizeWindow unit

fullscreenWindow ∷ ∀ r. Run (LUNAPARK r) Unit
fullscreenWindow = liftLunapark $ FullscreenWindow unit

executeScript ∷ ∀ r. LT.Script → Run (LUNAPARK r) J.Json
executeScript script = liftLunapark $ ExecuteScript script identity

executeScriptAsync ∷ ∀ r. LT.Script → Run (LUNAPARK r) J.Json
executeScriptAsync script = liftLunapark $ ExecuteScriptAsync script identity

getAllCookies ∷ ∀ r. Run (LUNAPARK r) (Array LT.Cookie)
getAllCookies = liftLunapark $ GetAllCookies identity

getCookie ∷ ∀ r. String → Run (LUNAPARK r) LT.Cookie
getCookie name = liftLunapark $ GetCookie name identity

addCookie ∷ ∀ r. LT.Cookie → Run (LUNAPARK r) Unit
addCookie cookie = liftLunapark $ AddCookie cookie unit

deleteCookie ∷ ∀ r. String → Run (LUNAPARK r) Unit
deleteCookie name = liftLunapark $ DeleteCookie name unit

deleteAllCookies ∷ ∀ r. Run (LUNAPARK r) Unit
deleteAllCookies = liftLunapark $ DeleteAllCookies unit

dismissAlert ∷ ∀ r. Run (LUNAPARK r) Unit
dismissAlert = liftLunapark $ DismissAlert unit

acceptAlert ∷ ∀ r. Run (LUNAPARK r) Unit
acceptAlert = liftLunapark $ AcceptAlert unit

getAlertText ∷ ∀ r. Run (LUNAPARK r) String
getAlertText = liftLunapark $ GetAlertText identity

sendAlertText ∷ ∀ r. String → Run (LUNAPARK r) Unit
sendAlertText txt = liftLunapark $ SendAlertText txt unit

screenshot ∷ ∀ r. String → Run (LUNAPARK r) Unit
screenshot fp = liftLunapark $ Screenshot fp unit

elementScreenshot ∷ ∀ r. LT.Element → String → Run (LUNAPARK r) Unit
elementScreenshot el fp = liftLunapark $ OnElement el $ ScreenshotEl fp unit

findElement ∷ ∀ r. LT.Locator → Run (LUNAPARK r) LT.Element
findElement l = liftLunapark $ FindElement l identity

findElements ∷ ∀ r. LT.Locator → Run (LUNAPARK r) (Array LT.Element)
findElements l = liftLunapark $ FindElements l identity

childElement ∷ ∀ r. LT.Element → LT.Locator → Run (LUNAPARK r) LT.Element
childElement el l = liftLunapark $ OnElement el $ ChildElement l identity

childElements ∷ ∀ r. LT.Element → LT.Locator → Run (LUNAPARK r) (Array LT.Element)
childElements el l = liftLunapark $ OnElement el $ ChildElements l identity

isSelected ∷ ∀ r. LT.Element → Run (LUNAPARK r) Boolean
isSelected el = liftLunapark $ OnElement el $ IsSelected identity

getAttribute ∷ ∀ r. LT.Element → String → Run (LUNAPARK r) String
getAttribute el name = liftLunapark $ OnElement el $ GetAttribute name identity

getProperty ∷ ∀ r. LT.Element → String → Run (LUNAPARK r) J.Json
getProperty el name = liftLunapark $ OnElement el $ GetProperty name identity

getCss ∷ ∀ r. LT.Element → String → Run (LUNAPARK r) String
getCss el name = liftLunapark $ OnElement el $ GetCss name identity

getText ∷ ∀ r. LT.Element → Run (LUNAPARK r) String
getText el = liftLunapark $ OnElement el $ GetText identity

getTagName ∷ ∀ r. LT.Element → Run (LUNAPARK r) String
getTagName el = liftLunapark $ OnElement el $ GetTagName identity

getRectangle ∷ ∀ r. LT.Element → Run (LUNAPARK r) LT.Rectangle
getRectangle el = liftLunapark $ OnElement el $ GetRectangle identity

isEnabled ∷ ∀ r. LT.Element → Run (LUNAPARK r) Boolean
isEnabled el = liftLunapark $ OnElement el $ IsEnabled identity

clickElement ∷ ∀ r. LT.Element → Run (LUNAPARK r) Unit
clickElement el = liftLunapark $ OnElement el $ ClickEl unit

clearElement ∷ ∀ r. LT.Element → Run (LUNAPARK r) Unit
clearElement el = liftLunapark $ OnElement el $ ClearEl unit

sendKeysElement ∷ ∀ r. LT.Element → String → Run (LUNAPARK r) Unit
sendKeysElement el txt = liftLunapark $ OnElement el $ SendKeysEl txt unit

isDisplayed ∷ ∀ r. LT.Element → Run (LUNAPARK r) Boolean
isDisplayed el = liftLunapark $ OnElement el $ IsDisplayed identity

submitElement ∷ ∀ r. LT.Element → Run (LUNAPARK r) Unit
submitElement el = liftLunapark $ OnElement el $ Submit unit

performActions ∷ ∀ r. LT.ActionRequest → Run (LUNAPARK r) Unit
performActions req = liftLunapark $ PerformActions req unit

releaseActions ∷ ∀ r. Run (LUNAPARK r) Unit
releaseActions = liftLunapark $ ReleaseActions unit
