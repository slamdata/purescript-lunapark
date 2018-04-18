module Lunapark.Endpoint where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Argonaut (Json)
import Data.Argonaut as J
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable as F
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Lunapark.Error as LE
import Lunapark.Types as LT
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as N
import Network.HTTP.StatusCode (StatusCode(..))


-- | Building whole endpoint leads to having tons of noizy things like `ElementFindElement` and `TouchClick`
-- | Making this a `Variant` is definitely overkill
-- | So, to have something like `/session/:sessId/element/:elId/doubleclick` we have
-- | `InSession sessId : InElement elId : DoubleClick : Nil`
-- | This is not as typesafe as it could be, but at least it saves from typos.
data EndpointPart
  = Session
  | InSession LT.SessionId
  | Status
  | Timeouts
  | Url
  | Back
  | Forward
  | Refresh
  | Title
  | Window
  | Handles
  | Frame
  | Parent
  | Rect
  | Maximize
  | Minimize
  | Fullscreen
  | Element
  | Elements
  | InElement LT.Element
  | Active
  | Selected
  | Attribute String
  | Property String
  | CssValue String
  | Text
  | Name
  | Enabled
  | Click
  | Clear
  | Value
  | Source
  | Execute
  | Sync
  | Async
  | Cookies
  | Cookie String
  | Actions
  | Alert
  | Dismiss
  | Accept
  | Screenshot
  | Displayed
  | AsyncScript
  | Sessions
  | WindowHandle
  | WindowHandles
  | ExecuteAsync
  | Size
  | Position
  | Submit
  | Keys
  | Location
  | AlertText
  | AcceptAlert
  | DismissAlert
  | MoveTo
  | ButtonDown
  | ButtonUp
  | DoubleClick
  | Touch
  | Down
  | Up
  | Move
  | Scroll
  | LongClick
  | Flick

type Endpoint = L.List EndpointPart

print ∷ Endpoint → String
print ps = "/" <> F.intercalate "/" (map printPart ps)

printPart ∷ EndpointPart → String
printPart = case _ of
  Session → "session"
  InSession s → "session/" <> un LT.SessionId s
  Status → "status"
  Timeouts → "timeouts"
  Url → "url"
  Back → "back"
  Forward → "forward"
  Refresh → "refresh"
  Title → "title"
  Window → "window"
  Handles → "handles"
  Frame → "frame"
  Parent → "parent"
  Rect → "rect"
  Maximize → "maximize"
  Minimize → "minimize"
  Fullscreen → "fullscreen"
  Element → "element"
  Elements → "elements"
  InElement e → "element/" <> un LT.Element e
  Active → "active"
  Attribute a → "attribute/" <> a
  Property p → "property/" <> p
  CssValue c → "css/" <> c
  Selected → "selected"
  Text → "text"
  Name → "name"
  Enabled → "enabled"
  Click → "click"
  Clear → "clear"
  Value → "value"
  Source → "source"
  Execute → "execute"
  Sync → "sync"
  Async → "async"
  Cookies → "cookie"
  Cookie c → "cookie/" <> c
  Actions → "actions"
  Alert → "alert"
  Dismiss → "dismiss"
  Accept → "accept"
  Screenshot → "screenshot"
  Displayed → "displayed"
  AsyncScript → "async_script"
  Sessions → "sessions"
  WindowHandle → "window_handle"
  WindowHandles → "window_handles"
  ExecuteAsync → "execute_async"
  Size → "size"
  Position → "position"
  Submit → "submit"
  Keys → "keys"
  AlertText → "alert_text"
  AcceptAlert → "accept_alert"
  MoveTo → "moveto"
  Location → "location"
  DismissAlert → "dismiss_alert"
  ButtonDown → "buttondown"
  ButtonUp → "buttonup"
  DoubleClick → "doubleclick"
  Touch → "touch"
  Down → "down"
  Up → "up"
  Move → "move"
  Scroll → "scroll"
  LongClick → "longclick"
  Flick → "flick"

handleAPIError ∷ N.AffjaxResponse Json → Either LE.Error Json
handleAPIError r = case r.status of
  StatusCode 200 → lmap LE.unknownError do
    obj ← J.decodeJson r.response
    obj J..? "value"
  code → Left $ LE.fromJson r.response

get ∷ ∀ e. String → Endpoint → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
get uri ep  = map handleAPIError $ N.get (uri <> print ep)

post ∷ ∀ e. String → Endpoint → Json → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
post uri ep obj = map handleAPIError $ N.post (uri <> print ep) obj

post_ ∷ ∀ e. String → Endpoint → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
post_ uri ep = map handleAPIError $ N.post' (uri <> print ep) (Nothing ∷ Maybe Unit)

delete ∷ ∀ e. String → Endpoint → Aff (ajax ∷ AJAX|e) (Either LE.Error Json)
delete uri ep = map handleAPIError $ N.delete (uri <> print ep)