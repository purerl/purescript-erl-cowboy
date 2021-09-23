-- | Bindings for `cowboy`.
-- |
-- | To construct a working cowboy application, the definitions here can be used with
-- | routing defined in `Erl.Cowboy.Routes`, and one of the handlers defind in submodules of
-- | `Erl.Cowboy.Handlers`. Core request processing is handled in `Erl.Cowboy.Req`.
module Erl.Cowboy 
(
  ProtocolOpts,
  dispatch,
  Env,
  startClear,
  startTls,
  stopListener,
  defaultOptions
) where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..), maybe, maybe')
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Cowboy.Routes (Dispatch)
import Erl.Data.List (List)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Kernel.Inet as Inet
import Erl.Kernel.Tcp as Tcp
import Erl.ModuleName (NativeModuleName)
import Erl.Ranch as Ranch
import Erl.Ssl as Ssl
import Foreign (Foreign, unsafeToForeign)
import Foreign as Foreign
import Record as Record
import Type.Proxy (Proxy(..))

foreign import startClear_ :: Atom -> Foreign -> Map Atom Foreign -> Effect (Either Foreign Unit)
foreign import startTls_ :: Atom -> Foreign -> Map Atom Foreign -> Effect (Either Foreign Unit)

type RanchOptions socketOpts  =
  ( 
    socket_opts :: Maybe (Record socketOpts)
  | 
  Ranch.Options
  )

defaultOptions :: forall a. Record (RanchOptions a)
defaultOptions = Record.insert (Proxy :: _ "socket_opts") Nothing Ranch.defaultOptions

type TcpOptions = Record (RanchOptions (Tcp.ListenOptions))
type SslOptions = Record (RanchOptions (Ssl.ListenOptions))

startClear :: Atom -> TcpOptions -> ProtocolOpts -> Effect (Either Foreign Unit)
startClear name options protoOpts =
  let 
    socketOptions = Inet.optionsToErl <<< Ranch.excludeOptions <$> options.socket_opts
    withoutSocketOptions = Record.delete (Proxy :: _ "socket_opts") options
    erlOptions = maybe' (\_ -> Ranch.optionsToErl withoutSocketOptions) (\opts -> Ranch.optionsToErl $ Record.insert (Proxy :: _ "socket_opts") opts withoutSocketOptions) socketOptions
  in
  startClear_ name erlOptions (convertProtocolOpts protoOpts)


startTls :: Atom -> SslOptions -> ProtocolOpts -> Effect (Either Foreign Unit)
startTls name options protoOpts =
  let 
    socketOptions = Inet.optionsToErl <<< Ranch.excludeOptions <$> options.socket_opts
    withoutSocketOptions = Record.delete (Proxy :: _ "socket_opts") options
    erlOptions = maybe' (\_ -> Ranch.optionsToErl withoutSocketOptions) (\opts -> Ranch.optionsToErl $ Record.insert (Proxy :: _ "socket_opts") opts withoutSocketOptions) socketOptions
  in
  startTls_ name erlOptions (convertProtocolOpts protoOpts)


foreign import stopListener :: Atom -> Effect Unit

type Env = Map Atom Foreign

type ProtocolOpts =
  { env :: Maybe Env
  , middlewares :: Maybe (List NativeModuleName)
  , streamHandlers :: Maybe (List NativeModuleName)
  }

dispatch :: Dispatch -> Env -> Env
dispatch = Map.insert (atom "dispatch") <<< Foreign.unsafeToForeign

convertProtocolOpts :: ProtocolOpts -> Map Atom Foreign
convertProtocolOpts { env, middlewares, streamHandlers } =
  Map.empty
    # opt "env" env
    # opt "middlewares" middlewares
    # opt "stream_handlers" streamHandlers
  where
  opt :: forall a. String -> Maybe a -> Map Atom Foreign -> Map Atom Foreign
  opt str = maybe identity (Map.insert (atom str) <<< unsafeToForeign) 
