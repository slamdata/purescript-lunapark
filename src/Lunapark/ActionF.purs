module Lunapark.ActionF where

import Prelude

import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds)
import Lunapark.Types as LT
import Run (Run)
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
type ActionsEffect r = ( lunaparkActions ∷ LUNAPARK_ACTIONS | r )

liftAction ∷ ∀ r. ActionF Unit → Run (ActionsEffect r) Unit
liftAction = R.lift _lunaparkActions

click ∷ ∀ r. LT.Button → Run (ActionsEffect r) Unit
click btn = liftAction $ Click btn unit

buttonDown ∷ ∀ r. LT.Button → Run (ActionsEffect r) Unit
buttonDown btn = liftAction $ ButtonDown btn unit

buttonUp ∷ ∀ r. LT.Button → Run (ActionsEffect r) Unit
buttonUp btn = liftAction $ ButtonUp btn unit

doubleClick ∷ ∀ r. LT.Button → Run (ActionsEffect r) Unit
doubleClick btn = liftAction $ DoubleClick btn unit

sendKeys ∷ ∀ r. String → Run (ActionsEffect r) Unit
sendKeys txt = liftAction $ SendKeys txt unit

moveTo ∷ ∀ r. LT.PointerMove → Run (ActionsEffect r) Unit
moveTo move = liftAction $ MoveTo move unit

pause ∷ ∀ r. Milliseconds → Run (ActionsEffect r) Unit
pause ms = liftAction $ Pause ms unit

tap ∷ ∀ r. Run (ActionsEffect r) Unit
tap = liftAction $ InTouch $ Tap unit

touchDown ∷ ∀ r. Run (ActionsEffect r) Unit
touchDown = liftAction $ InTouch $ TouchDown unit

touchUp ∷ ∀ r. Run (ActionsEffect r) Unit
touchUp = liftAction $ InTouch $ TouchUp unit

longTap ∷ ∀ r. Run (ActionsEffect r) Unit
longTap = liftAction $ InTouch $ LongClick unit

flick ∷ ∀ r. LT.PointerMove → Run (ActionsEffect r) Unit
flick move = liftAction $ InTouch $ Flick move unit

scroll ∷ ∀ r. LT.PointerMove → Run (ActionsEffect r) Unit
scroll move = liftAction $ InTouch $ Scroll move unit

doubleTap ∷ ∀ r. Run (ActionsEffect r) Unit
doubleTap = liftAction $ InTouch $ DoubleTap unit
