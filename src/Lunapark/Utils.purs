module Lunapark.Utils where

import Prelude

import Data.Argonaut.Decode (JsonDecodeError) as J
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Effect.Aff (Aff)
import Lunapark.Error as LE
import Run as R
import Run.Except as RE
import Unsafe.Coerce (unsafeCoerce)

liftAndRethrow
  ∷ ∀ r ε ω
  . Aff (Either ε ω)
  → R.Run (aff ∷ R.AFF, except ∷ RE.EXCEPT ε|r) ω
liftAndRethrow a = do
  res ← R.liftAff a
  RE.rethrow res

rethrowAsJsonDecodeError
  ∷ ∀ r
  . Either J.JsonDecodeError
  ~> R.Run (except ∷ RE.EXCEPT LE.Error|r)
rethrowAsJsonDecodeError =
  RE.rethrow <<< lmap LE.JsonDecodeError

-- Safe, since we actually want handler and result have same rows not, remove except
catch
  ∷ ∀ e r a
  . R.Run (except ∷ RE.EXCEPT e|r) a
  → (e → R.Run (except ∷ RE.EXCEPT e|r) a)
  → R.Run (except ∷ RE.EXCEPT e|r) a
catch = unsafeCoerce $ flip RE.catch
