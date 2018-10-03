module Lunapark.API where

import Prelude

import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut.Decode.Class (decodeJson) as J
import Data.Argonaut.Encode.Class (encodeJson) as J
import Data.FoldableWithIndex as FI
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..), isRight)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as Str
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable as T
import Data.Variant as V
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Foreign.Object as FO
import Lunapark.ActionF (_lunaparkActions, LUNAPARK_ACTIONS, ActionF(..), TouchF(..))
import Lunapark.Endpoint as LP
import Lunapark.Error as LE
import Lunapark.LunaparkF (_lunapark, LUNAPARK, ElementF(..), LunaparkF(..), performActions, findElement)
import Lunapark.Types as LT
import Lunapark.Utils (liftAndRethrow, throwLeft, catch)
import Node.Buffer as B
import Node.FS.Aff as FS
import Run as R
import Run.Except (EXCEPT)

init
  ∷ ∀ m r a
  . MonadAff m
  ⇒ MonadRec m
  ⇒ String
  → LT.CapabilitiesRequest
  → m (Either LE.Error (Lunapark r a → BaseRun r a))
init uri caps = do
  res ←
    liftAff
      $ LP.post uri (LP.Session : Nil)
      $ LT.encodeCapabilitiesRequest caps

  let
    sessionResponse = do
      sessObj ← res
      lmap LE.unknownError $ LT.decodeCreateSessionResponse sessObj

  T.for sessionResponse \{ session, capabilities } → do
    timeoutsRef ←
      liftEffect $ Ref.new
        { implicit: Milliseconds 0.0
        , pageLoad: Milliseconds 300000.0
        , script: Milliseconds 30000.0
        }

    requestMapRef ← liftEffect $ Ref.new Map.empty

    actionsEnabled ←
      map isRight
        $ liftAff
        $ LP.post uri  (LP.InSession session : LP.Actions : Nil)
        $ LT.encodeActionRequest $ FO.singleton "action-test"
        $ LT.NoSource [ LT.pause $ Milliseconds 0.0 ]

    let
      input =
        { timeoutsRef
        , requestMapRef
        , uri
        , session
        , capabilities
        , actionsEnabled
        }

    pure $ interpret input

interpret
  ∷ ∀ r
  . HandleLunaparkInput
  → Lunapark r
  ~> BaseRun r
interpret input = runLunapark input <<< runLunaparkActions input


type Lunapark r = BaseRun (lunapark ∷ LUNAPARK, lunaparkActions ∷ LUNAPARK_ACTIONS|r)

type BaseRun r = R.Run
  ( except ∷ EXCEPT LE.Error
  , aff ∷ R.AFF
  , effect ∷ R.EFFECT
  | r)

runLunapark ∷ ∀ r. HandleLunaparkInput → BaseRun (lunapark ∷ LUNAPARK|r) ~> BaseRun r
runLunapark input = do
  R.interpretRec (R.on _lunapark (handleLunapark input) R.send)

runLunaparkActions ∷ ∀ r. HandleLunaparkInput → Lunapark r ~> BaseRun (lunapark ∷ LUNAPARK|r)
runLunaparkActions input
  | input.actionsEnabled = interpretW3CActions Nil
  | otherwise = R.interpretRec (R.on _lunaparkActions (jsonWireActions input) R.send)

interpretW3CActions
  ∷ ∀ r
  . List LT.ActionSequence
  → Lunapark r
  ~> BaseRun (lunapark ∷ LUNAPARK|r)
interpretW3CActions acc as = case R.peel as of
  Left la → case tag la of
    Left a → w3cActions acc interpretW3CActions a
    Right others → do
      T.for_ (L.reverse acc) \s → performActions $ FO.singleton "dummy" s
      cont ← R.send others
      interpretW3CActions Nil cont
  Right a → pure a
  where
  tag = R.on _lunaparkActions Left Right

w3cActions
  ∷ ∀ r a
  . List LT.ActionSequence
  → ( List LT.ActionSequence
    → Lunapark r
    ~> BaseRun (lunapark ∷ LUNAPARK|r)
    )
  → ActionF (Lunapark r a)
  → BaseRun (lunapark ∷ LUNAPARK|r) a
w3cActions acc loop = case _ of
  Click btn next →
    let seq = [ LT.pointerDown btn, LT.pointerUp btn ]
    in loop (inMouse seq) next
  ButtonDown btn next →
    let seq = [ LT.pointerDown btn ]
    in loop (inMouse seq) next
  ButtonUp btn next →
    let seq = [ LT.pointerUp btn ]
    in loop (inMouse seq) next
  DoubleClick btn next →
    let seq = [ LT.pointerDown btn, LT.pointerUp btn, LT.pointerDown btn, LT.pointerUp btn ]
    in loop (inMouse seq) next
  MoveTo move next →
    let seq = [ LT.pointerMove move ]
    in loop (inPointer seq) next
  SendKeys txt next →
    let seq = T.foldMap (\ch → [ LT.keyDown ch, LT.keyUp ch ]) $ Str.toCharArray txt
    in loop (inKeys seq) next
  Pause ms next →
    let seq = [ LT.pause ms ]
    in loop (anywhere seq) next
  InTouch tch → case tch of
    Tap next →
      let seq = [ LT.pointerDown LT.LeftBtn, LT.pointerUp LT.LeftBtn ]
      in loop (inTouch seq) next
    TouchDown next →
      let seq = [ LT.pointerDown LT.LeftBtn ]
      in loop (inTouch seq) next
    TouchUp next →
      let seq = [ LT.pointerUp LT.LeftBtn ]
      in loop (inTouch seq) next
    LongClick next →
      let seq = [ LT.pointerDown LT.LeftBtn, LT.pause (Milliseconds 3000.0), LT.pointerUp LT.LeftBtn ]
      in loop (inTouch seq) next
    Flick move next →
      let seq = [ LT.pointerMove move ]
      in loop (inTouch seq) next
    Scroll move next →
      let seq = [ LT.pointerMove move ]
      in loop (inTouch seq) next
    DoubleTap next →
      let seq = [ LT.pointerDown LT.LeftBtn, LT.pointerUp LT.LeftBtn, LT.pointerDown LT.LeftBtn, LT.pointerUp LT.LeftBtn ]
      in loop (inTouch seq) next
  where
  unconsed = L.uncons acc

  inMouse seq = case unconsed of
    Just { head: (LT.Pointer LT.Mouse as), tail } →
      LT.Pointer LT.Mouse (as <> seq) : tail
    _ → LT.Pointer LT.Mouse seq : acc

  inPointer seq = case unconsed of
    Just { head: (LT.Pointer ptr as), tail } →
      LT.Pointer ptr (as <> seq) : tail
    _ → LT.Pointer LT.Mouse seq : acc

  inKeys seq = case unconsed of
    Just { head: LT.Key as, tail } →
      LT.Key (as <> seq) : tail
    _ → LT.Key seq : acc

  inTouch seq = case unconsed of
    Just { head: (LT.Pointer LT.Touch as), tail } →
      LT.Pointer LT.Touch (as <> seq) : tail
    _ → LT.Pointer LT.Mouse seq : acc

  anywhere ∷ Array (V.Variant (pause ∷ Milliseconds)) → L.List LT.ActionSequence
  anywhere seq = case unconsed of
    Nothing → L.singleton $ LT.NoSource seq
    Just { head, tail } → case head of
      LT.Pointer ptr as → LT.Pointer ptr (as <> map V.expand seq) : tail
      LT.Key as → LT.Key (as <> map V.expand seq) : tail
      LT.NoSource as → LT.NoSource (as <> seq) : tail

type HandleLunaparkInput =
  { session ∷ LT.SessionId
  , timeoutsRef ∷ Ref.Ref LT.Timeouts
  , requestMapRef ∷ Ref.Ref (Map.Map String Int)
  , uri ∷ String
  , capabilities ∷ Array LT.Capability
  , actionsEnabled ∷ Boolean
  }

jsonWireActions ∷ ∀ r. HandleLunaparkInput → ActionF ~> BaseRun (lunapark ∷ LUNAPARK|r)
jsonWireActions inp = case _ of
  Click btn next → do
    _ ← post (LP.Click : Nil) (LT.encodeButton btn)
    pure next
  ButtonDown btn next → do
    _ ← post (LP.ButtonDown : Nil) (LT.encodeButton btn)
    pure next
  ButtonUp btn next → do
    _ ← post (LP.ButtonUp : Nil) (LT.encodeButton btn)
    pure next
  DoubleClick btn next → do
    _ ← case btn of
      LT.LeftBtn → post_ (LP.DoubleClick : Nil)
      other → do
        _ ← post (LP.Click : Nil) (LT.encodeButton btn)
        post (LP.Click : Nil) (LT.encodeButton btn)
    pure next
  SendKeys txt next → do
    _ ← post (LP.Keys : Nil) (LT.encodeSendKeysRequest txt)
    pure next
  MoveTo move next → do
    element ← case move.origin of
      LT.FromViewport → map Just $ findElement $ LT.ByTagName "body"
      LT.FromPointer → pure Nothing
      LT.FromElement el → pure $ Just el
    let req = { xoffset: move.x, yoffset: move.y, element }
    _ ← post (LP.MoveTo : Nil) (LT.encodeMoveToRequest req)
    pure next
  Pause ms next → do
    R.liftAff $ Aff.delay ms
    pure next
  InTouch tch → case tch of
    Tap next → do
      _ ← post_ (LP.Touch : LP.Click : Nil)
      pure next
    TouchDown next → do
      _ ← post_ (LP.Touch : LP.Down : Nil)
      pure next
    TouchUp next → do
      _ ← post_ (LP.Touch : LP.Up : Nil)
      pure next
    LongClick next → do
      _ ← post_ (LP.Touch : LP.LongClick : Nil)
      pure next
    Flick move next → do
      element ← case move.origin of
        LT.FromViewport → map Just $ findElement $ LT.ByTagName "body"
        LT.FromPointer → pure Nothing
        LT.FromElement el → pure $ Just el
      let req = { xoffset: move.x, yoffset: move.y, element }
      _ ← post (LP.Touch : LP.Flick : Nil) (LT.encodeMoveToRequest req)
      pure next
    Scroll move next → do
      element ← case move.origin of
        LT.FromViewport → map Just $ findElement $ LT.ByTagName "body"
        LT.FromPointer → pure Nothing
        LT.FromElement el → pure $ Just el
      let req = { xoffset: move.x, yoffset: move.y, element }
      _ ← post (LP.Touch : LP.Scroll : Nil) (LT.encodeMoveToRequest req)
      pure next
    DoubleTap next → do
      _ ← post_ (LP.Touch : LP.DoubleClick : Nil)
      pure next
  where
  post a b = liftAndRethrow $ LP.post inp.uri (inSession : a) b
  post_ a = liftAndRethrow $ LP.post_ inp.uri (inSession : a)

  inSession ∷ LP.EndpointPart
  inSession = LP.InSession inp.session

handleLunapark ∷ ∀ r. HandleLunaparkInput → LunaparkF ~> BaseRun r
handleLunapark inp = case _ of
  Quit next → do
    _ ← delete $ inSession : Nil
    pure next
  Status cont → do
    res ← get $ LP.Status : Nil
    ss ← throwLeft $ LT.decodeServerStatus res
    pure $ cont ss
  GetTimeouts cont → do
    res ← R.liftEffect $ Ref.read inp.timeoutsRef
    pure $ cont res
  SetTimeouts ts next → do
    R.liftEffect $ Ref.write ts inp.timeoutsRef
    tryAndCache "set timeouts"
      [ void $ post (inSession : LP.Timeouts : Nil) (LT.encodeTimeouts ts)
      , do T.for_ (LT.encodeLegacyTimeouts ts) \j →
           void $ post (inSession : LP.Timeouts : Nil) j
      ]
    pure next
  GoTo uri next → do
    _ ← post (inSession : LP.Url : Nil) $ LT.encodeGoRequest uri
    pure next
  GetUrl cont → do
    res ← get $ inSession : LP.Url : Nil
    map cont $ throwLeft $ J.decodeJson res
  Back next → do
    _ ← post_ (inSession : LP.Back : Nil)
    pure next
  Forward next → do
    _ ← post_ (inSession : LP.Forward : Nil)
    pure next
  Refresh next → do
    _ ← post_ (inSession : LP.Refresh : Nil)
    pure next
  GetTitle cont → do
    res ← get (inSession : LP.Title : Nil)
    map cont $ throwLeft $ J.decodeJson res
  GetWindowHandle cont → do
    res ← tryAndCache "get window handle"
      [ get (inSession : LP.Window : Nil)
      , get (inSession : LP.WindowHandle : Nil)
      ]
    map cont $ throwLeft $ LT.decodeWindowHandle res
  GetWindowHandles cont → do
    res ← tryAndCache "get window handles"
      [ get (inSession : LP.Window : LP.Handles : Nil)
      , get (inSession : LP.WindowHandles : Nil)
      ]
    map cont $ throwLeft $ T.traverse LT.decodeWindowHandle =<< J.decodeJson res
  CloseWindow next → do
    _ ← delete (inSession : LP.Window : Nil)
    pure next
  SwitchToWindow w next → do
    _ ← post (inSession : LP.Window : Nil) (LT.encodeSwitchToWindowRequest w)
    pure next
  SwitchToFrame fid next → do
    _ ← post (inSession : LP.Frame : Nil) (LT.encodeFrameId fid)
    pure next
  SwitchToParentFrame next → do
    _ ← post_ (inSession : LP.Frame : LP.Parent : Nil)
    pure next
  GetWindowRectangle cont → do
    res ← tryAndCache "get window rectangle"
      [ do res ← get (inSession : LP.Window : LP.Rect : Nil)
           throwLeft $ LT.decodeRectangle res
      , do position ← get (inSession : LP.Window : LP.Position : Nil)
           size ← get (inSession : LP.Window : LP.Size : Nil)
           throwLeft $ LT.decodeRectangleLegacy { position, size }
      ]
    pure $ cont res
  SetWindowRectangle r next → do
    tryAndCache "set window rectangle"
      [ void $ post (inSession : LP.Window : LP.Rect : Nil) (LT.encodeRectangle r)
      , do let js = LT.encodeRectangleLegacy r
           _ ← post (inSession : LP.Window : LP.Size : Nil) js.size
           void $ post (inSession : LP.Window : LP.Position : Nil) js.position
      ]
    pure next
  MaximizeWindow next → do
    _ ← post_ (inSession : LP.Window : LP.Maximize : Nil)
    pure next
  MinimizeWindow next → do
    _ ← post_ (inSession : LP.Window : LP.Minimize : Nil)
    pure next
  FullscreenWindow next → do
    _ ← post_ (inSession : LP.Window : LP.Fullscreen : Nil)
    pure next
  ExecuteScript script cont → do
    map cont $ tryAndCache "execute script"
      [ post (inSession : LP.Execute : LP.Sync : Nil) (LT.encodeScript script)
      , post (inSession : LP.Execute : Nil) (LT.encodeScript script)
      ]
  ExecuteScriptAsync script cont → do
    map cont $ tryAndCache "execute script async"
      [ post (inSession : LP.Execute : LP.Async : Nil) (LT.encodeScript script)
      , post (inSession : LP.ExecuteAsync : Nil) (LT.encodeScript script)
      ]
  GetAllCookies cont → do
    res ← get (inSession : LP.Cookies : Nil)
    map cont $ throwLeft $ T.traverse LT.decodeCookie =<< J.decodeJson res
  GetCookie name cont → do
    res ← get (inSession : LP.Cookie name : Nil)
    map cont $ throwLeft $ LT.decodeCookie res
  DeleteAllCookies next → do
    _ ← delete (inSession : LP.Cookies : Nil)
    pure next
  DeleteCookie name next → do
    _ ← delete (inSession : LP.Cookie name : Nil)
    pure next
  AddCookie cookie next → do
    _ ← post (inSession : LP.Cookies : Nil) (LT.encodeCookie cookie)
    pure next
  DismissAlert next → do
    _ ← tryAndCache "dismiss alert"
      [ post_ (inSession : LP.Alert : LP.Dismiss : Nil)
      , post_ (inSession : LP.DismissAlert : Nil)
      ]
    pure next
  AcceptAlert next → do
    _ ← tryAndCache "accept alert"
      [ post_ (inSession : LP.Alert : LP.Accept : Nil)
      , post_ (inSession : LP.AcceptAlert : Nil)
      ]
    pure next
  GetAlertText cont → do
    res ← tryAndCache "get alert text"
      [ get (inSession : LP.Alert : LP.Text : Nil)
      , get (inSession : LP.AlertText : Nil)
      ]
    map cont $ throwLeft $ J.decodeJson res
  SendAlertText str next → do
    _ ← tryAndCache "send alert text"
      [ post (inSession : LP.Alert : LP.Text : Nil) (LT.encodeSendKeysRequest str)
      , post (inSession : LP.AlertText : Nil) (LT.encodeSendKeysRequest str)
      ]
    pure next
  Screenshot fp next → do
    res ← get (inSession : LP.Screenshot : Nil)
    screenshotPack ← throwLeft $ LT.decodeScreenshot res
    buffer ← R.liftEffect $ B.fromString screenshotPack.content screenshotPack.encoding
    R.liftAff $ FS.writeFile fp buffer
    pure next
  FindElement loc cont → do
    res ← post (inSession : LP.Element : Nil) (LT.encodeLocator loc)
    map cont $ throwLeft $ LT.decodeElement res
  FindElements loc cont → do
    res ← post (inSession : LP.Elements : Nil) (LT.encodeLocator loc)
    map cont $ throwLeft $ T.traverse LT.decodeElement =<< J.decodeJson res
  GetActiveElement cont → do
    res ← get (inSession : LP.Element : LP.Active : Nil)
    map cont $ throwLeft $ LT.decodeElement res
  PerformActions req next → do
    when inp.actionsEnabled
      $ void $ post
        (inSession : LP.Actions : Nil)
        (LT.encodeActionRequest req)
    pure next
  ReleaseActions next → do
    when inp.actionsEnabled $ void $ delete (inSession : LP.Actions : Nil)
    pure next
  OnElement el elF →
    let inElement = LP.InElement el
    in case elF of
      ChildElement loc cont → do
        res ← post (inSession : inElement : LP.Element : Nil) (LT.encodeLocator loc)
        map cont $ throwLeft $ LT.decodeElement res
      ChildElements loc cont → do
        res ← post (inSession : inElement : LP.Elements : Nil) (LT.encodeLocator loc)
        map cont $ throwLeft $ T.traverse LT.decodeElement =<< J.decodeJson res
      ScreenshotEl fp next → do
        res ← get (inSession : inElement : LP.Screenshot : Nil)
        screenshotPack ← throwLeft $ LT.decodeScreenshot res
        buffer ← R.liftEffect $ B.fromString screenshotPack.content screenshotPack.encoding
        R.liftAff $ FS.writeFile fp buffer
        pure next
      IsSelected cont → do
        res ← get (inSession : inElement : LP.Selected : Nil)
        map cont $ throwLeft $ J.decodeJson res
      GetAttribute attr cont → do
        res ← get (inSession : inElement : LP.Attribute attr : Nil)
        map cont $ throwLeft $ J.decodeJson res
      GetProperty prop cont → do
        map cont $ get (inSession : inElement : LP.Property prop : Nil)
      GetCss css cont → do
        res ← get (inSession : inElement : LP.CssValue css : Nil)
        map cont $ throwLeft $ J.decodeJson res
      GetText cont → do
        res ← get (inSession : inElement : LP.Text : Nil)
        map cont $ throwLeft $ J.decodeJson res
      GetTagName cont → do
        res ← get (inSession : inElement : LP.Name : Nil)
        map cont $ throwLeft $ J.decodeJson res
      GetRectangle cont →
        map cont $ tryAndCache "get element rectangle"
          [ do res ← get (inSession : inElement : LP.Rect : Nil)
               throwLeft $ LT.decodeRectangle res
          , do position ← get (inSession : inElement : LP.Position : Nil)
               size ← get (inSession : inElement : LP.Size : Nil)
               throwLeft $ LT.decodeRectangleLegacy { position, size }
          ]
      IsEnabled cont → do
        res ← get (inSession : inElement : LP.Enabled : Nil)
        map cont $ throwLeft $ J.decodeJson res
      ClickEl next → do
        _ ← post_ (inSession : inElement : LP.Click : Nil)
        pure next
      ClearEl next → do
        _ ← post_ (inSession : inElement : LP.Clear : Nil)
        pure next
      SendKeysEl txt next → do
        _ ← tryAndCache "send keys chromedriver hack"
          [ post (inSession : inElement : LP.Value : Nil) (LT.encodeSendKeysRequest txt)
          , post (inSession : inElement : LP.Value : Nil)
              $ J.encodeJson $ FO.singleton "value" $ Str.toCharArray txt
          ]
        pure next
      IsDisplayed cont → do
        res ← tryAndCache "is element displayed"
          [ get (inSession : inElement : LP.Displayed : Nil)
          , do let script =
                     { script: """var el = arguments[0]; return el.offsetHeight > 0 && el.offsetWidth > 0"""
                     , args: [ LT.encodeElement el ]
                     }
               handleLunapark inp $ ExecuteScript script identity
          ]
        map cont $ throwLeft $ J.decodeJson res
      Submit next → do
        _ ← post_ (inSession : inElement : LP.Submit : Nil)
        pure next

  where
  delete a = liftAndRethrow $ LP.delete inp.uri a
  post a b = liftAndRethrow $ LP.post inp.uri a b
  get a = liftAndRethrow $ LP.get inp.uri a
  post_ a = liftAndRethrow $ LP.post_ inp.uri a

  tryAndCache ∷ ∀ a. String → Array (BaseRun r a) → BaseRun r a
  tryAndCache key actions = do
    let emptyCases = throwLeft $ Left $ "No valid cases for " <> key <> " caching"
    let incorrectCache = throwLeft $ Left $ "Fallback for " <> key <> " error"

    mp ← R.liftEffect $ Ref.read inp.requestMapRef
    case Map.lookup key mp of
      Just ix → case A.index actions ix of
        Just action → action
        Nothing → incorrectCache
      Nothing →
        let
          go ix acc act =
            let try' = do
                  a ← act
                  R.liftEffect $ Ref.modify_ (Map.insert key ix) inp.requestMapRef
                  pure a
            in catch try' \_ → acc
        in
         FI.foldlWithIndex go emptyCases actions

  inSession ∷ LP.EndpointPart
  inSession = LP.InSession inp.session
