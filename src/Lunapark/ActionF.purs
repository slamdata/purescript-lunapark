module Lunapark.ActionF where

import Prelude

import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds)
import Lunapark.Types as LT
import Run as R


data ActionF a
  = Click LT.Button a
  | ButtonDown LT.Button a
  | ButtonUp LT.Button a
  | DoubleClick LT.Button a
  | SendKeys String a
  | MoveTo LT.PointerMove a
  | InTouch (TouchF a)
  | Pause Milliseconds a

data TouchF a
  = Tap a
  | TouchDown a
  | TouchUp a
  | LongClick a
  | Flick LT.PointerMove a
  | Scroll LT.PointerMove a
  | DoubleTap a

derive instance functorActionF ∷ Functor ActionF
derive instance functorTouchF ∷ Functor TouchF

_lunaparkActions = SProxy ∷ SProxy "lunaparkActions"
type LUNAPARK_ACTIONS = R.FProxy ActionF
type WithAction r = R.Run (lunaparkActions ∷ LUNAPARK_ACTIONS|r) Unit

liftAction ∷ ∀ r. ActionF Unit → WithAction r
liftAction = R.lift _lunaparkActions

click ∷ ∀ r. LT.Button → WithAction r
click btn = liftAction $ Click btn unit

buttonDown ∷ ∀ r. LT.Button → WithAction r
buttonDown btn = liftAction $ ButtonDown btn unit

buttonUp ∷ ∀ r. LT.Button → WithAction r
buttonUp btn = liftAction $ ButtonUp btn unit

doubleClick ∷ ∀ r. LT.Button → WithAction r
doubleClick btn = liftAction $ DoubleClick btn unit

sendKeys ∷ ∀ r. String → WithAction r
sendKeys txt = liftAction $ SendKeys txt unit

moveTo ∷ ∀ r. LT.PointerMove → WithAction r
moveTo move = liftAction $ MoveTo move unit

pause ∷ ∀ r. Milliseconds → WithAction r
pause ms = liftAction $ Pause ms unit

tap ∷ ∀ r. WithAction r
tap = liftAction $ InTouch $ Tap unit

touchDown ∷ ∀ r. WithAction r
touchDown = liftAction $ InTouch $ TouchDown unit

touchUp ∷ ∀ r. WithAction r
touchUp = liftAction $ InTouch $ TouchUp unit

longTap ∷ ∀ r. WithAction r
longTap = liftAction $ InTouch $ LongClick unit

flick ∷ ∀ r. LT.PointerMove → WithAction r
flick move = liftAction $ InTouch $ Flick move unit

scroll ∷ ∀ r. LT.PointerMove → WithAction r
scroll move = liftAction $ InTouch $ Scroll move unit

doubleTap ∷ ∀ r. WithAction r
doubleTap = liftAction $ InTouch $ DoubleTap unit
