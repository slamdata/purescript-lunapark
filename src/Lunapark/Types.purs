module Lunapark.Types where

import Prelude

import Control.Alt ((<|>))
import CSS.Selector as CSS
import CSS.Render as CSSR
import Data.Argonaut (Json)
import Data.Argonaut as J
import Data.Time.Duration (Milliseconds(..))
import Data.Either (Either(..))
import Data.Newtype (class Newtype, un)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Data.XPath.Types as X
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
  map Element $ obj J..? "element-6066-11e4-a52e-4f735466cecf" <|> obj J..? "ELEMENT"

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
  session ← decodeSessionId =<< obj J..? "sessionId"
  capabilities ← decodeCapabilities =<< obj J..? "capabilities"
  pure { session, capabilities }

type ServerStatus =
  { ready ∷ Boolean
  , message ∷ String
  }

decodeServerStatus ∷ Json → Either String ServerStatus
decodeServerStatus = J.decodeJson >=> \obj → { ready: _, message: _ } <$> obj J..? "ready" <*> obj J..? "message"

type Timeouts =
  { script ∷ Milliseconds
  , pageLoad ∷ Milliseconds
  , implicit ∷ Milliseconds
  }

decodeTimeouts ∷ Json → Either String Timeouts
decodeTimeouts = J.decodeJson >=> \obj → do
  script ← map Milliseconds $ obj J..? "script"
  pageLoad ← map Milliseconds $ obj J..? "pageLoad"
  implicit ← map Milliseconds $ obj J..? "implicit"
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
  width ← obj J..? "width"
  height ← obj J..? "height"
  x ← obj J..? "x"
  y ← obj J..? "y"
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
