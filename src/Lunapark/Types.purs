module Lunapark.Types where

import Prelude

import Control.Alt ((<|>))
import CSS.Selector as CSS
import CSS.Render as CSSR
import Data.Argonaut (Json, (.?))
import Data.Argonaut as J
import Data.Array as A
import Data.Foldable as F
import Data.Time.Duration (Milliseconds(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Data.XPath.Types as X
import Node.Encoding as NE
import Unsafe.Coerce (unsafeCoerce)

newtype SessionId = SessionId String
derive instance newtypeSessionId ∷ Newtype SessionId _

newtype WindowHandle = WindowHandle String
derive instance newtypeWindowHandle ∷ Newtype WindowHandle _

data FrameId = ByElementId String | ByIndex Int | TopFrame

newtype Element = Element String
derive instance newtypeElementId ∷ Newtype Element _

decodeElement ∷ Json → Either String Element
decodeElement = J.decodeJson >=> \obj →
  map Element $ obj .? "element-6066-11e4-a52e-4f735466cecf" <|> obj .? "ELEMENT"

encodeElement ∷ Element → Json
encodeElement (Element eid) = J.encodeJson $ SM.fromFoldable
  [ Tuple "element-6066-11e4-a52e-4f735466cecf" eid
  , Tuple "ELEMENT" eid
  ]

data Capabilities

encodeCapabilities ∷ Capabilities → Json
encodeCapabilities = unsafeCoerce

decodeCapabilities ∷ Json → Either String Capabilities
decodeCapabilities js = Left "unimplemented"

decodeSessionId ∷ Json → Either String SessionId
decodeSessionId = map SessionId <<< J.decodeJson

type CreateSessionResponse =
 { session  ∷ SessionId
 , capabilities ∷ Capabilities
 }

decodeCreateSessionResponse ∷ Json → Either String CreateSessionResponse
decodeCreateSessionResponse = J.decodeJson >=> \obj → do
  session ← decodeSessionId =<< obj .? "sessionId"
  capabilities ← decodeCapabilities =<< obj .? "capabilities"
  pure { session, capabilities }

type ServerStatus =
  { ready ∷ Boolean
  , message ∷ String
  }

decodeServerStatus ∷ Json → Either String ServerStatus
decodeServerStatus = J.decodeJson >=> \obj → { ready: _, message: _ } <$> obj .? "ready" <*> obj .? "message"

type Timeouts =
  { script ∷ Milliseconds
  , pageLoad ∷ Milliseconds
  , implicit ∷ Milliseconds
  }

decodeTimeouts ∷ Json → Either String Timeouts
decodeTimeouts = J.decodeJson >=> \obj → do
  script ← map Milliseconds $ obj .? "script"
  pageLoad ← map Milliseconds $ obj .? "pageLoad"
  implicit ← map Milliseconds $ obj .? "implicit"
  pure { script, pageLoad, implicit }

encodeTimeouts ∷ Timeouts → Json
encodeTimeouts r = J.encodeJson $ SM.fromFoldable
  [ Tuple "script" (un Milliseconds r.script)
  , Tuple "pageLoad" (un Milliseconds r.pageLoad)
  , Tuple "implicit" (un Milliseconds r.implicit)
  ]

encodeGoRequest ∷ String → Json
encodeGoRequest url = J.encodeJson $ SM.fromFoldable [ Tuple "url" url ]

decodeWindowHandle ∷ Json → Either String WindowHandle
decodeWindowHandle = map WindowHandle <<< J.decodeJson

encodeSwitchToWindowRequest ∷ WindowHandle → Json
encodeSwitchToWindowRequest w = J.encodeJson $ SM.fromFoldable [ Tuple "handle" $ un WindowHandle w ]

encodeFrameId ∷ FrameId → Json
encodeFrameId fid = J.encodeJson $ SM.fromFoldable [ Tuple "id" encoded ]
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
  width ← obj .? "width"
  height ← obj .? "height"
  x ← obj .? "x"
  y ← obj .? "y"
  pure { width, height, x, y }

encodeRectangle ∷ Rectangle → Json
encodeRectangle r = J.encodeJson $ SM.fromFoldable
  [ Tuple "width" r.width
  , Tuple "height" r.height
  , Tuple "x" r.x
  , Tuple "y" r.y
  ]

data Locator
  = ByCss CSS.Selector
  | ByXPath X.XPath
  | ByTagName String
  | ByLinkText String
  | ByPartialLinkText String

encodeLocator ∷ Locator → Json
encodeLocator l = J.encodeJson $ SM.fromFoldable case l of
  ByCss sel →
    [ Tuple "using" "css selector"
    , Tuple "value" $ CSSR.selector sel
    ]
  ByXPath sel →
    [ Tuple "using" "xpath"
    , Tuple "value" $ X.printXPath sel
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

encodeSendKeysRequest ∷ String → Json
encodeSendKeysRequest txt = J.encodeJson $ SM.fromFoldable [ Tuple "text" txt ]

type Script =
  { script ∷ String
  , args ∷ Array Json
  }

encodeScript ∷ Script → Json
encodeScript r = J.encodeJson $ SM.fromFoldable
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
encodeCookie r = J.encodeJson $ SM.fromFoldable
  [ Tuple "cookie" $ SM.fromFoldable
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
  name ← obj .? "name"
  value ← obj .? "value"
  path ← maybify $ obj .? "path"
  domain ← maybify $ obj .? "domain"
  secure ← maybify $ obj .? "secure"
  httpOnly ← maybify $ obj .? "httpOnly"
  expiry ← maybify $ obj .? "expiry"
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
