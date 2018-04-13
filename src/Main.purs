module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff as Aff
import Control.Monad.Except.Trans as ET
import Control.Monad.Reader.Trans as RT
import Control.Monad.State.Trans as ST
import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, extend, (<<=), (=>>))
import Data.Newtype (wrap)
import CSS as CSS
import CSS.Selector as Selector
import Data.Foldable (for_)
import Data.Either (Either(..), either)
import Data.StrMap as SM
import Data.Map as Map
import Data.Argonaut as A
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax as AJ
import Node.ChildProcess as CP
import Data.Posix.Signal (Signal(..))
import Control.Monad.Eff.Now as Now
import Run as R
import Node.Process as PR

import Debug.Trace as DT

import Lunapark.Starter as LS
import Lunapark.Types as LT
import Lunapark.API as LA
import Lunapark.Helpers as LH

defaultTimeouts ∷ LT.Timeouts
defaultTimeouts =
  { pageLoad: wrap 300000.0
  , implicit: wrap 0.0
  , script: wrap 30000.0
  }

main ∷ Eff _ Unit
main = void $ Aff.launchAff do
  LS.start (Map.singleton LT.Chrome "ch.exe") "selenium-3.11.0.jar" [ ]

  let baseURI = "http://localhost:4444/wd/hub"
  ei ← LA.init baseURI { alwaysMatch: [ ], firstMatch: [ [ LT.BrowserName LT.Chrome ] ] }
  case ei of
    Left err → do
      DT.traceAnyA "An error occured"
      DT.traceAnyA err

    Right sr → do
      let config = { session: sr.session, baseURI, capabilities: sr.capabilities }
      res ←

        R.runBaseAff' $ R.runReader config $ R.runExcept $ R.runState {timeouts: defaultTimeouts} do
--        flip RT.runReaderT config $ ET.runExceptT $ flip ST.runStateT { timeouts: defaultTimeouts } do
          LA.go "http://google.com"
          DT.traceAnyA "TS"
          LH.getTimeouts >>= DT.traceAnyA
          el ← LA.findElement $ LT.ByCss $ CSS.fromString "a"
          LH.setTimeouts { pageLoad: wrap 1000.0, script: wrap 10000.0, implicit: wrap 10000.0 }
          DT.traceAnyA =<< liftEff Now.now
          ts ← LH.getTimeouts
          DT.traceAnyA =<< liftEff Now.now
          visible ← LH.isVisible el
          LH.screenshot "./everything.png"
          LH.elementScreenshot el "./a.png"
          DT.traceAnyA el
          DT.traceAnyA visible
      _ ← R.runBaseAff' $ R.runReader config $ R.runExcept $ R.runState { timeouts: defaultTimeouts } $ LA.quit
--          LA.quit
      DT.traceAnyA res
  void $ liftEff $ PR.exit 0 --CP.kill SIGTERM cp
