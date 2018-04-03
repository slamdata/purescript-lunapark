module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Aff as Aff
import Data.Foldable (for_)
import Data.Either (Either(..))
import Data.StrMap as SM
import Data.Argonaut as A
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax as AJ

import Debug.Trace as DT

main ∷ Eff _ Unit
main = void $ Aff.launchAff do
  res1 ← AJ.get "http://localhost:4444/wd/hub/status"
  res2 ← AJ.post "http://localhost:4444/wd/hub/session" $ A.encodeJson $ SM.fromFoldable
    [ Tuple "desiredCapabilities" $ SM.fromFoldable [ Tuple "browserName" "MicrosoftEdge" ]
    ]

  let
    eitherSessionId ∷ Either String String
    eitherSessionId = do
        obj ← A.decodeJson res2.response
        obj A..? "sessionId"

  for_ eitherSessionId \sessId → do
    res3 ← AJ.post ("http://localhost:4444/wd/hub/session/" <> sessId <> "/url") $ A.encodeJson $ SM.fromFoldable
      [ Tuple "url" "https://www.w3.org/TR/webdriver/" ]
    DT.traceAnyA res3
    DT.traceAnyA $ res3.response ∷ A.Json

    res4 ← AJ.post ("http://localhost:4444/wd/hub/session/" <> sessId <> "/element") $ A.encodeJson $ SM.fromFoldable
      [ Tuple "using" "css selector"
      , Tuple "value" "#respecDocument"
      ]

    DT.traceAnyA "ELEMENT "
    DT.traceAnyA $ res4.response ∷ A.Json

    res6 ← AJ.post ("http://localhost:4444/wd/hub/session/" <> sessId <> "/element") $ A.encodeJson $ SM.fromFoldable
      [ Tuple "using" "css selector"
      , Tuple "value" "title "
      ]

    DT.traceAnyA "ELEMENT "
    DT.traceAnyA $ res6.response ∷ A.Json

    DT.traceAnyA "========================================"

    res5 ← AJ.delete ("http://localhost:4444/wd/hub/session/" <> sessId)
    DT.traceAnyA res5
    DT.traceAnyA $ res5.response ∷ A.Json

  DT.traceAnyA res1
  DT.traceAnyA $ res1.response ∷ String
  DT.traceAnyA res2
  DT.traceAnyA $ res2.response ∷ A.Json
  DT.traceAnyA "HEHEY!"
