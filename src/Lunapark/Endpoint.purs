-- | Building whole endpoint leads to having tons of noizy things like `ElementFindElement` and `TouchClick`
-- | Making this a `Variant` is definitely overkill
-- | So, to have something like `/session/:sessId/element/:elId/doubleclick` we have
-- | `InSession sessId : InElement elId : DoubleClick : Nil`
-- | This is not as typesafe as it could be, but at least it saves from typos.
module Lunapark.Endpoint
  ( delete
  , get
  , post
  , post'
  , EndpointPart(..)
  )
  where

import Prelude

import Affjax as N
import Affjax.RequestBody as NQ
import Affjax.ResponseFormat as NR
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (decodeJson) as J
import Data.Argonaut.Decode.Combinators ((.:)) as J
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable as F
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect.Aff (Aff)
import Lunapark.Error as LE
import Lunapark.Types as LT
import Lunapark.WebDriverError as LWE

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

handleAPIError
  ∷ Either N.Error (N.Response Json)
  → Either LE.Error Json
handleAPIError (Left error) = Left $ LE.AffjaxError error
handleAPIError (Right r) = case r.status of
  StatusCode 200 → lmap LE.JsonDecodeError do
    obj ← J.decodeJson r.body
    obj J..: "value"
  _ →
    Left $ either LE.JsonDecodeError LE.WebDriverError $ LWE.fromJson r.body

get ∷ String → Endpoint → Aff (Either LE.Error Json)
get uri ep  = map handleAPIError $ N.get NR.json (uri <> print ep)

post ∷ String → Endpoint → Json → Aff (Either LE.Error Json)
post uri ep obj = map handleAPIError $ N.post NR.json (uri <> print ep) $ Just (NQ.json obj)

post' ∷ String → Endpoint → Aff (Either LE.Error Json)
post' uri ep = map handleAPIError $ N.post NR.json (uri <> print ep) Nothing

delete ∷ String → Endpoint → Aff (Either LE.Error Json)
delete uri ep = map handleAPIError $ N.delete NR.json (uri <> print ep)
