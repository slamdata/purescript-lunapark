module Lunapark.Error where

import Prelude

import Data.Argonaut as J
import Data.Either (Either(..), either)

data ErrorType
  = ElementClickIntercepted
  | ElementNotSelectable
  | ElementNotInteractable
  | InsecureCertificate
  | InvalidArgument
  | InvalidCookieDomain
  | InvalidCoordinates
  | InvalidElementState
  | InvalidSelector
  | InvalidSessionId
  | JavaScriptError
  | MoveTargetOutOfBounds
  | NoSuchAlert
  | NoSuchCookie
  | NoSuchElement
  | NoSuchFrame
  | NoSuchWindow
  | ScriptTimeout
  | SessionNotCreated
  | StaleElementReference
  | Timeout
  | UnableToSetCookie
  | UnableToCaptureScreen
  | UnexpectedAlertOpen
  | UnknownCommand
  | UnknownError
  | UnknownMethod
  | UnsupportedOperation

fromStringCode ∷ String → Either String ErrorType
fromStringCode = case _ of
  "element click intercepted" → Right ElementClickIntercepted
  "element not selectable" → Right ElementNotSelectable
  "element not interactable" → Right ElementNotInteractable
  "insecure certificate" → Right InsecureCertificate
  "invalid argument" → Right InvalidArgument
  "invalid cookie domain" → Right InvalidCookieDomain
  "invalid coordinates" → Right InvalidCoordinates
  "invalid element state" → Right InvalidElementState
  "invalid selector" → Right InvalidSelector
  "invalid session id" → Right InvalidSessionId
  "javascript error" → Right JavaScriptError
  "move target out of bounds" → Right MoveTargetOutOfBounds
  "no such alert" → Right NoSuchAlert
  "no such cookie" → Right NoSuchCookie
  "no such element" → Right NoSuchElement
  "no such frame" → Right NoSuchFrame
  "no such window" → Right NoSuchWindow
  "script timeout" → Right ScriptTimeout
  "session not created" → Right SessionNotCreated
  "stale element reference" → Right StaleElementReference
  "timeout" → Right Timeout
  "unable to set cookie" → Right UnableToSetCookie
  "unable to capture screen" → Right UnableToCaptureScreen
  "unexpected alert open" → Right UnexpectedAlertOpen
  "unknown command" → Right UnknownCommand
  "unknown error" → Right UnknownError
  "unknown method" → Right UnknownMethod
  "unsupported operation" → Right UnsupportedOperation
  s → Left s

toStringCode ∷ ErrorType → String
toStringCode = case _ of
  ElementClickIntercepted → "element click intercepted"
  ElementNotSelectable → "element not selectable"
  ElementNotInteractable → "element not interactable"
  InsecureCertificate → "insecure certificate"
  InvalidArgument → "invalid argument"
  InvalidCookieDomain → "invalid cookie domain"
  InvalidCoordinates → "invalid coordinates"
  InvalidElementState → "invalid element state"
  InvalidSelector → "invalid selector"
  InvalidSessionId → "invalid session id"
  JavaScriptError → "javascript error"
  MoveTargetOutOfBounds → "move target out of bounds"
  NoSuchAlert → "no such alert"
  NoSuchCookie → "no such cookie"
  NoSuchElement → "no such element"
  NoSuchFrame → "no such frame"
  NoSuchWindow → "no such window"
  ScriptTimeout → "script timeout"
  SessionNotCreated → "session not created"
  StaleElementReference → "stale element reference"
  Timeout → "timeout"
  UnableToSetCookie → "unable to set cookie"
  UnableToCaptureScreen → "unable to capture screen"
  UnexpectedAlertOpen → "unexpected alert open"
  UnknownCommand → "unknown command"
  UnknownError → "unknown error"
  UnknownMethod → "unknown method"
  UnsupportedOperation → "unsupported operation"

type Error =
  { error ∷ ErrorType
  , message ∷ String
  , stacktrace ∷ String
  }

fromJson ∷ J.Json → Error
fromJson js = either unknownError identity do
  obj ← J.decodeJson js
  value ← obj J..? "value"
  error ← fromStringCode =<< value J..? "error"
  message ← value J..? "message"
  stacktrace ← value J..? "stacktrace"
  pure { error, message, stacktrace }

unknownError ∷ String → Error
unknownError message =
  { error: UnknownError
  , message
  , stacktrace: ""
  }
