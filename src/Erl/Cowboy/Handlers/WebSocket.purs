-- | Types and helpers for a cowboy_websocket Websockets callback module
-- |
-- | See `Erl.Cowboy.Handlers`.
-- |
-- | Example:
-- | ```purescript
-- | _behaviour :: CowboyWebsocketBehaviour
-- | _behaviour = cowboyWebsocketBehaviour { init, websocket_handle, websocket_info }
-- | 
-- | data Config
-- | data HandlerState
-- |
-- | init :: InitHandler Config HandlerState
-- | init = ...
-- | websocket_handle :: FrameHandler HandlerState 
-- | websocket_handle = ...
-- | websocket_info :: InfoHandler HandlerState
-- | websocket_info = ...
-- | ```
module Erl.Cowboy.Handlers.WebSocket (
  InitResult
, InitHandler
, initResult
, FrameHandler
, Frame(..)
, InFrame
, decodeInFrame
, OutFrame
, outFrame
, CallResult
, okResult
, hibernateResult
, replyResult
, replyAndHibernateResult
, stopResult
, WSInitHandler
, InfoHandler
, TerminateHandler
, TerminateReason(..)
, RemotePayload(..)
, TerminateError(..)
, CloseCode(..)
, RawTerminateReason
, decodeReason
, PartialReq
, CowboyWebsocketBehaviour
, cowboyWebsocketBehaviour
) where

import Attribute (Attribute(..), Behaviour)
import Data.Function.Uncurried (Fn4, mkFn4)
import Data.Maybe (Maybe(..))
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3)
import Erl.Atom (Atom)
import Erl.Cowboy.Handlers.Common (CrashType(..), TerminateResult) as C
import Erl.Cowboy.Req (Req)
import Erl.Data.Binary (Binary)
import Erl.Data.List (List)

foreign import data InitResult :: Type -> Type

foreign import initResult :: forall a. a -> Req -> InitResult a

-- | Init handler to upgrade to cowboy_websocket -- init()
type InitHandler c s = EffectFn2 Req c (InitResult s)

data Frame =
    TextFrame String
  | BinaryFrame Binary
  | PingFrame Binary
  | PongFrame Binary

foreign import data InFrame :: Type

foreign import decodeInFrameImpl ::
  (String -> Frame)
  -> (Binary -> Frame)
  -> (Binary -> Frame)
  -> (Binary -> Frame)
  -> InFrame
  -> Frame

decodeInFrame :: InFrame -> Frame
decodeInFrame = decodeInFrameImpl TextFrame BinaryFrame PingFrame PongFrame

foreign import data OutFrame :: Type

foreign import encodeOutFrameImpl :: (Fn4
    (String -> OutFrame)
    (Binary -> OutFrame)
    (Binary -> OutFrame)
    (Binary -> OutFrame)
    OutFrame)
  -> OutFrame

-- | TODO: Can also output iodata() versions, but input always binary
outFrame :: Frame -> OutFrame
outFrame frame = encodeOutFrameImpl (mkFn4 \text binary ping pong ->
  case frame of
    TextFrame x -> text x
    BinaryFrame x -> binary x
    PingFrame x -> ping x
    PongFrame x -> pong x
)

foreign import data CallResult :: Type -> Type

foreign import okResult :: forall s. s -> CallResult s

foreign import hibernateResult :: forall s. s -> CallResult s

foreign import replyResult :: forall s. s -> List OutFrame -> CallResult s

foreign import replyAndHibernateResult :: forall s. s -> List OutFrame -> CallResult s

foreign import stopResult :: forall s. s -> CallResult s

-- | Optional WS init handler (post-upgrade run in websocket process - websocket_init())
type WSInitHandler s = EffectFn1 s (CallResult s)

-- | Main frame handler - websocket_handle()
type FrameHandler s = EffectFn2 InFrame s (CallResult s)

-- | Handler for erlang info messages - websocket_info()
type InfoHandler a s = EffectFn2 a s (CallResult s)

-- | Cowboy does not provide the full Req object to terminate, so currently completely opaque
foreign import data PartialReq :: Type

-- | ws:close_code() :: 1000..1003 | 1006..1011 | 3000..4999
newtype CloseCode = CloseCode Int
data RemotePayload = RemotePayload CloseCode Binary

data TerminateError =
    BadEncoding
  | BadFrame
  | Closed
  | OtherError Atom
data TerminateReason =
    Normal
  | Remote (Maybe RemotePayload)
  | Stop
  | Timeout
  | Crash C.CrashType
  | Error TerminateError 

-- | This is different from other callback modules terminate reasons
foreign import data RawTerminateReason :: Type

foreign import decodeReasonImpl
  :: TerminateReason
  -> (Maybe RemotePayload -> TerminateReason)
  -> (RemotePayload -> Maybe RemotePayload)
  -> Maybe RemotePayload
  -> (CloseCode -> Binary -> RemotePayload)
  -> TerminateReason
  -> TerminateReason
  -> (C.CrashType -> TerminateReason)
  -> C.CrashType
  -> C.CrashType
  -> C.CrashType
  -> (TerminateError -> TerminateReason)
  -> TerminateError
  -> TerminateError
  -> TerminateError
  -> (Atom -> TerminateError)
  -> RawTerminateReason
  -> TerminateReason

decodeReason :: RawTerminateReason -> TerminateReason
decodeReason = decodeReasonImpl Normal Remote Just Nothing RemotePayload Stop Timeout Crash C.Error C.Exit C.Throw Error BadEncoding BadFrame Closed OtherError

type TerminateHandler s = EffectFn3 TerminateReason PartialReq s C.TerminateResult

type CowboyWebsocketBehaviour = Behaviour "cowboy_websocket"

cowboyWebsocketBehaviour :: forall a i s.
  { init :: InitHandler a s
  , websocket_handle :: FrameHandler s
  , websocket_info :: InfoHandler i s
  } -> CowboyWebsocketBehaviour
cowboyWebsocketBehaviour _ = Attribute