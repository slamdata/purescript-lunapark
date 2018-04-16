module Lunapark.Starter where

import Prelude

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (liftEff)
import Lunapark.Types as LT
import Data.Traversable as T
import Node.Path (FilePath)
import Node.FS (FS)
import Node.FS.Aff as FS
import Node.ChildProcess as CP
import Unsafe.Coerce (unsafeCoerce)

start
  ∷ ∀ e m
  . MonadAff (fs ∷ FS, cp ∷ CP.CHILD_PROCESS|e) m
  ⇒ LT.DriverPaths
  → FilePath
  → Array String
  → m Unit
start drivers jarPath additionalParams = do
  preparedJar ← liftAff $ FS.realpath jarPath
  preparedDrivers ← liftAff $ T.traverse FS.realpath drivers

  liftEff $ CP.exec
    ("java "
     <> T.intercalate " " (LT.renderDriverPaths preparedDrivers)
     <> " -jar " <> preparedJar <> " "
     <> T.intercalate " " additionalParams)
    CP.defaultExecOptions
    (const $ pure unit)
