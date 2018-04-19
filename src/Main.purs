module Main where

import Prelude

import Control.Monad.Aff as Aff
import Control.Monad.Eff (Eff)
import Data.Newtype (wrap)
import Data.Traversable (for_)
import Lunapark.API as LA
import Lunapark.Types as LT
import Run as R
import Run.Except as RE

import Debug.Trace as DT

main ∷ Eff _ Unit
main = void $ Aff.launchAff do
  eiInterpret ← LA.init "http://localhost:4444/wd/hub" { alwaysMatch: [], firstMatch: [ [ LT.BrowserName LT.Chrome ] ] }
  for_ eiInterpret \interpret → do
    R.runBaseAff' $ RE.runExcept $ interpret do
      st ← LA.status
      DT.traceAnyA "status"
      DT.traceAnyA st
      ts ← LA.getTimeouts
      DT.traceAnyA "timeouts"
      DT.traceAnyA ts
      LA.setTimeouts ts{ pageLoad = wrap 10.0 }
      ts' ← LA.getTimeouts
      DT.traceAnyA "timeouts'"
      DT.traceAnyA ts'
      LA.setTimeouts ts
      LA.quit
