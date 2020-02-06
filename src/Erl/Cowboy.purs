-- | Bindings for `cowboy`.
-- |
-- | To construct a working cowboy application, the definitions here can be used with 
-- | routing defined in `Erl.Cowboy.Routes`, and one of the handlers defind in submodules of
-- | `Erl.Cowboy.Handlers`. Core request processing is handled in `Erl.Cowboy.Req`.
module Erl.Cowboy (
  TransOpt(..),
  ProtoOpt(..),
  ProtocolOpts,
  protocolOpts,
  dispatch,
  Env,
  startClear
) where

import Prelude

import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Cowboy.Routes (Dispatch)
import Erl.Data.List (List)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple4)
import Erl.ModuleName (NativeModuleName)
import Foreign (Foreign)
import Foreign as Foreign

foreign import startClear :: Atom -> List TransOpt -> ProtocolOpts -> Effect Unit

type Env = Map Atom Foreign

data TransOpt = Ip (Tuple4 Int Int Int Int) | Port Int
data ProtoOpt
  = Env Env
  | Middlewares (List NativeModuleName)
  | StreamHandlers (List NativeModuleName)

dispatch :: Dispatch -> Env -> Env
dispatch = Map.insert (atom "dispatch") <<< Foreign.unsafeToForeign

foreign import data ProtocolOpts :: Type

foreign import protocolOpts :: List ProtoOpt -> ProtocolOpts

