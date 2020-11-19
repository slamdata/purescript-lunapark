module Lunapark.Error where

import Prelude
import Affjax as N
import Lunapark.WebDriverError as LWE
import Data.Argonaut.Decode (JsonDecodeError, printJsonDecodeError) as J

data CachingError
  = EmptyCases String
  | IncorrectCache String

data Error
  = JsonDecodeError J.JsonDecodeError
  | WebDriverError LWE.WebDriverError
  | AffjaxError N.Error
  | CachingError CachingError

printError ∷ Error → String
printError = case _ of
  JsonDecodeError jsonDecodeError →
    J.printJsonDecodeError jsonDecodeError
  WebDriverError webdriverError →
    "Response with error message:\n"
    <> "  error type: " <> LWE.toStringCode webdriverError.error <> "\n"
    <> "  message: " <> webdriverError.message <> "\n"
    <> "  stacktrace: " <> webdriverError.stacktrace
  AffjaxError affjaxError →
    N.printError affjaxError
  CachingError cachingError →
    "Error during caching:\n  " <> printCachingError cachingError
  where
    printCachingError = case _ of
      EmptyCases key → "There is no working implementation for " <> key <> " action."
      IncorrectCache key → "Trying another implementation for " <> key <> " action."
