-- | This module contains types for requests and response.
-- | Most of them are records with `α → Json` and `Json → Either String α` functions
-- | not newtypes with `Encode|DecodeJson` instances.
module Lunapark.Types where

import Prelude

import CSS.Render as CSSR
import CSS.Selector as CSS
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (Json, jsonEmptyObject, jsonNull) as J
import Data.Argonaut.Decode.Class (decodeJson) as J
import Data.Argonaut.Decode.Combinators ((.:))
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson) as J
import Data.Argonaut.Encode.Combinators (extend) as J
import Data.Array as A
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.String as Str
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable as F
import Data.Tuple (Tuple(..))
import Data.Variant as V
import Data.XPath.AST as XA
import Data.XPath.Printer (printSelector)
import Foreign.Object as FO
import Node.Encoding as NE
import Node.Path (FilePath)

newtype SessionId = SessionId String

derive instance newtypeSessionId ∷ Newtype SessionId _
derive newtype instance eqSessionId ∷ Eq SessionId
derive newtype instance ordSessionId ∷ Ord SessionId

newtype WindowHandle = WindowHandle String

derive instance newtypeWindowHandle ∷ Newtype WindowHandle _
derive newtype instance eqWindowHandle ∷ Eq WindowHandle
derive newtype instance ordWindowHandle ∷ Ord WindowHandle

currentWindow ∷ WindowHandle
currentWindow = WindowHandle "current"

data FrameId = ByElementId String | ByIndex Int | TopFrame

derive instance eqFrameId ∷ Eq FrameId
derive instance ordFrameId ∷ Ord FrameId

newtype Element = Element String

derive instance newtypeElement ∷ Newtype Element _
derive newtype instance eqElement ∷ Eq Element
derive newtype instance ordElement ∷ Ord Element

decodeElement ∷ Json → Either String Element
decodeElement = J.decodeJson >=> \obj →
  map Element $ obj .: "element-6066-11e4-a52e-4f735466cecf" <|> obj .: "ELEMENT"

encodeElement ∷ Element → Json
encodeElement (Element eid) = J.encodeJson $ FO.fromFoldable
  [ Tuple "element-6066-11e4-a52e-4f735466cecf" eid
  , Tuple "ELEMENT" eid
  ]

decodeSessionId ∷ Json → Either String SessionId
decodeSessionId = map SessionId <<< J.decodeJson

type CreateSessionResponse =
 { session  ∷ SessionId
 , capabilities ∷ Array Capability
 }

decodeCreateSessionResponse ∷ Json → Either String CreateSessionResponse
decodeCreateSessionResponse = J.decodeJson >=> \obj → do
  session ← decodeSessionId =<< obj .: "sessionId"
  capabilities ← decodeCapabilities =<< obj .: "capabilities"
  pure { session, capabilities }

type ServerStatus =
  { ready ∷ Boolean
  , message ∷ String
  }

decodeServerStatus ∷ Json → Either String ServerStatus
decodeServerStatus = J.decodeJson >=> \obj → { ready: _, message: _ } <$> obj .: "ready" <*> obj .: "message"

type Timeouts =
  { script ∷ Milliseconds
  , pageLoad ∷ Milliseconds
  , implicit ∷ Milliseconds
  }

decodeTimeouts ∷ Json → Either String Timeouts
decodeTimeouts = J.decodeJson >=> \obj → do
  script ← map Milliseconds $ obj .: "script"
  pageLoad ← map Milliseconds $ obj .: "pageLoad"
  implicit ← map Milliseconds $ obj .: "implicit"
  pure { script, pageLoad, implicit }

encodeTimeouts ∷ Timeouts → Json
encodeTimeouts r = J.encodeJson $ FO.fromFoldable
  [ Tuple "script" (un Milliseconds r.script)
  , Tuple "pageLoad" (un Milliseconds r.pageLoad)
  , Tuple "implicit" (un Milliseconds r.implicit)
  ]

encodeLegacyTimeouts ∷ Timeouts → Array Json
encodeLegacyTimeouts r =
  [ J.encodeJson $ FO.singleton "script" $ un Milliseconds r.script
  , J.encodeJson $ FO.singleton "implicit" $ un Milliseconds r.implicit
  , J.encodeJson $ FO.singleton "page load" $ un Milliseconds r.pageLoad
  ]

encodeGoRequest ∷ String → Json
encodeGoRequest url = J.encodeJson $ FO.fromFoldable [ Tuple "url" url ]

decodeWindowHandle ∷ Json → Either String WindowHandle
decodeWindowHandle = map WindowHandle <<< J.decodeJson

encodeSwitchToWindowRequest ∷ WindowHandle → Json
encodeSwitchToWindowRequest w = J.encodeJson $ FO.fromFoldable [ Tuple "handle" $ un WindowHandle w ]

encodeFrameId ∷ FrameId → Json
encodeFrameId fid = J.encodeJson $ FO.fromFoldable [ Tuple "id" encoded ]
  where
  encoded = case fid of
    TopFrame → J.jsonNull
    ByIndex ix → J.encodeJson ix
    ByElementId eid → J.encodeJson eid

type Rectangle =
  { width ∷ Int
  , height ∷ Int
  , x ∷ Int
  , y ∷ Int
  }

decodeRectangle ∷ Json → Either String Rectangle
decodeRectangle = J.decodeJson >=> \obj → do
  width ← obj .: "width"
  height ← obj .: "height"
  x ← obj .: "x"
  y ← obj .: "y"
  pure { width, height, x, y }

decodeRectangleLegacy ∷ { size ∷ Json, position ∷ Json }  → Either String Rectangle
decodeRectangleLegacy { size, position } = do
  sobj ← J.decodeJson size
  pobj ← J.decodeJson position
  x ← pobj .: "x"
  y ← pobj .: "y"
  width ← sobj .: "width"
  height ← sobj .: "height"
  pure { width, height, x, y }

encodeRectangleLegacy ∷ Rectangle → { size ∷ Json, position ∷ Json }
encodeRectangleLegacy r =
  { size: J.encodeJson $ FO.fromFoldable [ Tuple "width" r.width, Tuple "height" r.height ]
  , position: J.encodeJson $ FO.fromFoldable [ Tuple "x" r.x, Tuple "y" r.y ]
  }

encodeRectangle ∷ Rectangle → Json
encodeRectangle r = J.encodeJson $ FO.fromFoldable
  [ Tuple "width" r.width
  , Tuple "height" r.height
  , Tuple "x" r.x
  , Tuple "y" r.y
  ]

data Locator
  = ByCss CSS.Selector
  | ByXPath XA.Selector
  | ByTagName String
  | ByLinkText String
  | ByPartialLinkText String
  | Raw RawLocator

type RawLocator =
  { using ∷ String
  , value ∷ String
  }

encodeLocator ∷ Locator → Json
encodeLocator l = J.encodeJson $ FO.fromFoldable case l of
  ByCss sel →
    [ Tuple "using" "css selector"
    , Tuple "value" $ CSSR.selector sel
    ]
  ByXPath sel →
    [ Tuple "using" "xpath"
    , Tuple "value" $ printSelector sel
    ]
  ByLinkText sel →
    [ Tuple "using" "link text"
    , Tuple "value" sel
    ]
  ByPartialLinkText sel →
    [ Tuple "using" "partial link text"
    , Tuple "value" sel
    ]
  ByTagName sel →
    [ Tuple "using" "tag name"
    , Tuple "value" sel
    ]
  Raw r →
    [ Tuple "using" r.using
    , Tuple "value" r.value
    ]

encodeSendKeysRequest ∷ String → Json
encodeSendKeysRequest txt = J.encodeJson $ FO.fromFoldable [ Tuple "text" txt ]

type Script =
  { script ∷ String
  , args ∷ Array Json
  }

encodeScript ∷ Script → Json
encodeScript r = J.encodeJson $ FO.fromFoldable
  [ Tuple "script" $ J.encodeJson r.script
  , Tuple "args" $ J.encodeJson r.args
  ]

type Cookie =
  { name ∷ String
  , value ∷ String
  , path ∷ Maybe String
  , domain ∷ Maybe String
  , secure ∷ Maybe Boolean
  , httpOnly ∷ Maybe Boolean
  , expiry ∷ Maybe Int
  }

encodeCookie ∷ Cookie → Json
encodeCookie r = J.encodeJson $ FO.fromFoldable
  [ Tuple "cookie" $ FO.fromFoldable
    $ [ Tuple "name" $ J.encodeJson r.name
      , Tuple "value" $ J.encodeJson r.value
      ]
    <> maybeToAOfPair "path" r.path
    <> maybeToAOfPair "domain" r.domain
    <> maybeToAOfPair "secure" r.secure
    <> maybeToAOfPair "httpOnly" r.httpOnly
    <> maybeToAOfPair "expiry" r.expiry
  ]
  where
  maybeToAOfPair ∷ ∀ a. J.EncodeJson a ⇒ String → Maybe a → Array (Tuple String J.Json)
  maybeToAOfPair key mb = F.foldMap (A.singleton <<< Tuple key <<< J.encodeJson) mb

decodeCookie ∷ Json → Either String Cookie
decodeCookie = J.decodeJson >=> \obj → do
  name ← obj .: "name"
  value ← obj .: "value"
  path ← maybify $ obj .: "path"
  domain ← maybify $ obj .: "domain"
  secure ← maybify $ obj .: "secure"
  httpOnly ← maybify $ obj .: "httpOnly"
  expiry ← maybify $ obj .: "expiry"
  pure
    { name
    , value
    , path
    , domain
    , secure
    , httpOnly
    , expiry
    }
  where
  maybify ∷ ∀ a b. Either a b → Either a (Maybe b)
  maybify e = map Just e <|> Right Nothing


type Screenshot =
  { content ∷ String
  , encoding ∷ NE.Encoding
  }

decodeScreenshot ∷ Json → Either String Screenshot
decodeScreenshot j =
  { content: _, encoding: NE.Base64 } <$> J.decodeJson j

data Button = LeftBtn | MiddleBtn | RightBtn

encodeButton ∷ Button → Json
encodeButton = J.encodeJson <<< case _ of
  LeftBtn → 0
  MiddleBtn → 1
  RightBtn → 2

data PointerMoveOrigin
  = FromViewport
  | FromPointer
  | FromElement Element

encodeOrigin ∷ PointerMoveOrigin → Json
encodeOrigin = case _ of
  FromViewport → J.encodeJson "viewport"
  FromPointer → J.encodeJson "pointer"
  FromElement el → encodeElement el

type PointerMove =
  { duration ∷ Milliseconds
  , origin ∷ PointerMoveOrigin
  , x ∷ Int
  , y ∷ Int
  }

encodePointerMove ∷ PointerMove → FO.Object Json
encodePointerMove r = FO.fromFoldable
  [ Tuple "x" $ J.encodeJson r.x
  , Tuple "y" $ J.encodeJson r.y
  , Tuple "duration" $ J.encodeJson $ un Milliseconds r.duration
  , Tuple "origin" $ encodeOrigin r.origin
  ]


type Action = V.Variant
  ( pause ∷ Milliseconds
  , keyDown ∷ Char
  , keyUp ∷ Char
  , pointerUp ∷ Button
  , pointerDown ∷ Button
  , pointerMove ∷ PointerMove
  )

pause ∷ ∀ r a. a → V.Variant (pause ∷ a|r)
pause = V.inj (SProxy ∷ SProxy "pause")

keyDown ∷ ∀ r a. a → V.Variant (keyDown ∷ a|r)
keyDown = V.inj (SProxy ∷ SProxy "keyDown")

keyUp ∷ ∀ r a. a → V.Variant (keyUp ∷ a|r)
keyUp = V.inj (SProxy ∷ SProxy "keyUp")

pointerUp ∷ ∀ r a. a → V.Variant (pointerUp ∷ a|r)
pointerUp = V.inj (SProxy ∷ SProxy "pointerUp")

pointerDown ∷ ∀ r a. a → V.Variant (pointerDown ∷ a|r)
pointerDown = V.inj (SProxy ∷ SProxy "pointerDown")

pointerMove ∷ ∀ r a. a → V.Variant (pointerMove ∷ a|r)
pointerMove = V.inj (SProxy ∷ SProxy "pointerMove")

encodeAction ∷ Action → Json
encodeAction = V.match
  { pause: \ms →
     J.encodeJson $ FO.fromFoldable
       [ Tuple "duration" $ J.encodeJson $ un Milliseconds ms
       , Tuple "type" $ J.encodeJson "pause"
       ]
  , keyDown: \ch →
     J.encodeJson $ FO.fromFoldable
       [ Tuple "value" $ J.encodeJson ch
       , Tuple "type" $ J.encodeJson "keyDown"
       ]
  , keyUp: \ch →
     J.encodeJson $ FO.fromFoldable
       [ Tuple "value" $ J.encodeJson ch
       , Tuple "type" $ J.encodeJson "keyUp"
       ]
  , pointerUp: \btn →
     J.encodeJson $ FO.fromFoldable
       [ Tuple "button" $ encodeButton btn
       , Tuple "type" $ J.encodeJson "pointerUp"
       ]
  , pointerDown: \btn →
     J.encodeJson $ FO.fromFoldable
       [ Tuple "button" $ encodeButton btn
       , Tuple "type" $ J.encodeJson "pointerDown"
       ]
  , pointerMove: \pm →
     J.encodeJson $ FO.insert "type" (J.encodeJson "pointerMove") $ encodePointerMove pm
  }


data PointerType = Mouse | Pen | Touch

printPointerType ∷ PointerType → String
printPointerType = case _ of
  Mouse → "mouse"
  Pen → "pen"
  Touch → "touch"

data ActionSequence
  = NoSource
      (Array (V.Variant (pause ∷ Milliseconds)))
  | Key
      (Array (V.Variant (keyDown ∷ Char, keyUp ∷ Char, pause ∷ Milliseconds)))
  | Pointer PointerType
      (Array (V.Variant (pause ∷ Milliseconds, pointerUp ∷ Button, pointerDown ∷ Button, pointerMove ∷ PointerMove)))

-- Right, this is not an `Array` but `StrMap` because all `ActionSequence`s are tagged with unique id's
type ActionRequest = FO.Object ActionSequence

encodeActionRequest ∷ ActionRequest → Json
encodeActionRequest sm = J.encodeJson $ FO.singleton "actions" $ map encodePair arrayOfPairs
  where
  arrayOfPairs ∷ Array (Tuple String ActionSequence)
  arrayOfPairs = FO.toUnfoldable sm

  encodePair ∷ Tuple String ActionSequence → Json
  encodePair (Tuple identifier sequence) =
    J.encodeJson $ FO.insert "id" (J.encodeJson identifier) $ encodeSequence sequence

  encodeSequence ∷ ActionSequence → FO.Object Json
  encodeSequence = case _ of
    NoSource as → FO.fromFoldable
      [ Tuple "type" $ J.encodeJson "none"
      , Tuple "actions" $ J.encodeJson $ map (encodeAction <<< V.expand) as
      ]
    Key as → FO.fromFoldable
      [ Tuple "type" $ J.encodeJson "key"
      , Tuple "actions" $ J.encodeJson $ map (encodeAction <<< V.expand) as
      ]
    Pointer ptype as → FO.fromFoldable
      [ Tuple "type" $ J.encodeJson "pointer"
      , Tuple "parameters" $ J.encodeJson $ FO.singleton "pointerType" $ printPointerType ptype
      , Tuple "actions" $ J.encodeJson $ map (encodeAction <<< V.expand) as
      ]

data BrowserType
  = MSEdge
  | Chrome
  | Firefox

derive instance eqBrowserType ∷ Eq BrowserType
derive instance ordBrowserType ∷ Ord BrowserType

type DriverPaths = Map.Map BrowserType FilePath

renderDriverPaths ∷ DriverPaths → Array String
renderDriverPaths ps = map renderDriverPath pairs
  where
  pairs ∷ Array (Tuple BrowserType String)
  pairs = Map.toUnfoldable ps

  renderDriverPath ∷ Tuple BrowserType String → String
  renderDriverPath (Tuple br path) = renderBrowserProp br <> "=\"" <> path <> "\""

  renderBrowserProp ∷ BrowserType → String
  renderBrowserProp = case _ of
    MSEdge → "-Dwebdriver.edge.driver"
    Chrome → "-Dwebdriver.chrome.driver"
    Firefox → "-DWebdriver.gecko.driver"

data PageLoad
  = Normal
  | Eager
  | Immediate

data UnhandledPrompt = Accept | Dismiss

data Platform = Windows | Mac | Linux | Any

data Capability
  = BrowserName BrowserType
  | BrowserVersion String
  | PlatformName Platform
  | AcceptInsecureCerts Boolean
  | PageLoadStrategy PageLoad
  | DesiredTimeouts Timeouts
  | UnhandledPromptBehavior UnhandledPrompt
  | CustomCapability String Json


encodeCapability ∷ Capability → Tuple String Json
encodeCapability = case _ of
  BrowserName bn → Tuple "browserName" $ J.encodeJson case bn of
    MSEdge → "MicrosoftEdge"
    Chrome → "chrome"
    Firefox → "firefox"
  BrowserVersion bv → Tuple "browserVersion" $ J.encodeJson bv
  PlatformName pn → Tuple "platform" $ J.encodeJson case pn of
    Windows → "WINDOWS"
    Mac → "MAC"
    Linux → "LINUX"
    Any → "ANY"
  AcceptInsecureCerts b → Tuple "acceptInsecureCerts" $ J.encodeJson b
  PageLoadStrategy s → Tuple "pageLoadStrategy" $ J.encodeJson case s of
    Normal → "normal"
    Eager → "eager"
    Immediate → "none"
  DesiredTimeouts t → Tuple "timeouts" $ encodeTimeouts t
  UnhandledPromptBehavior p → Tuple "unhandledPromptBehavior" $ J.encodeJson case p of
    Accept → "accept"
    Dismiss → "dismiss"
  CustomCapability k v → Tuple k v

encodeCapabilities ∷ ∀ f. F.Foldable f ⇒ f Capability → Json
encodeCapabilities = F.foldl (\b a → J.extend (encodeCapability a) b) J.jsonEmptyObject

decodeCapabilities ∷ Json → Either String (Array Capability)
decodeCapabilities = J.decodeJson >=> \obj →
  F.for (FO.toUnfoldable obj) \l@(Tuple key val) →
    decodeCapability l <|> Right (CustomCapability key val)
  where
  decodeCapability ∷ Tuple String Json → Either String Capability
  decodeCapability (Tuple key val) = case key of
    "browserName" → BrowserName <$> decodeBrowserName val
    "browserVersion" → BrowserVersion <$> J.decodeJson val
    "acceptInsecureCerts" → AcceptInsecureCerts <$> J.decodeJson val
    "pageLoadStrategy" → PageLoadStrategy <$> decodePageLoadStrategy val
    "timeouts" → DesiredTimeouts <$> decodeTimeouts val
    "unhandledPromptBehaviour" → UnhandledPromptBehavior <$> decodePrompt val
    _ → Left "unhandled"

  decodeBrowserName ∷ Json → Either String BrowserType
  decodeBrowserName = J.decodeJson >=> \str → case Str.toLower str of
    "microsoftedge" → Right MSEdge
    "chrome" → Right Chrome
    "firefox" → Right Firefox
    _ → Left "unhandled"

  decodePageLoadStrategy ∷ Json → Either String PageLoad
  decodePageLoadStrategy = J.decodeJson >=> \str → case Str.toLower str of
    "none" → Right Immediate
    "normal" → Right Normal
    "eager" → Right Eager
    _ → Left "unhandled"

  decodePrompt ∷ Json → Either String UnhandledPrompt
  decodePrompt = J.decodeJson >=> \str → case Str.toLower str of
    "accept" → Right Accept
    "dismiss" → Right Dismiss
    _ → Left "unhandled"

type CapabilitiesRequest =
  { alwaysMatch ∷ Array Capability
  , firstMatch ∷ Array (Array Capability)
  }

encodeCapabilitiesRequest ∷ CapabilitiesRequest → Json
encodeCapabilitiesRequest r = J.encodeJson $ FO.singleton "capabilities" $ FO.fromFoldable
  [ Tuple "alwaysMatch" $ encodeCapabilities r.alwaysMatch
  , Tuple "firstMatch" $ J.encodeJson $ map encodeCapabilities r.firstMatch
  ]

type MoveToRequest =
  { element ∷ Maybe Element
  , xoffset ∷ Int
  , yoffset ∷ Int
  }

encodeMoveToRequest ∷ MoveToRequest → Json
encodeMoveToRequest r = J.encodeJson $ FO.fromFoldable
  [ Tuple "element" $ case r.element of
       Nothing → J.jsonNull
       Just el → J.encodeJson $ un Element el
  , Tuple "xoffset" $ J.encodeJson r.xoffset
  , Tuple "yoffset" $ J.encodeJson r.yoffset
  ]
