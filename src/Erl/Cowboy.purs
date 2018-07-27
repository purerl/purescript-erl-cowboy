module Erl.Cowboy (
  TransOpt(..),
  ProtoOpt(..),
  ProtocolOpts,
  protocolOpts,
  ProtoEnv(..),
  ProtocolEnv,
  env,
  startClear
) where

import Prelude

import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Cowboy.Routes (Dispatch, Module)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple4)

foreign import startClear :: Atom -> List TransOpt -> ProtocolOpts -> Effect Unit

data TransOpt = Ip (Tuple4 Int Int Int Int) | Port Int
data ProtoOpt = Env ProtocolEnv | Middlewares (List Module)
data ProtoEnv = Dispatch Dispatch | Fn (Effect Unit)

foreign import data ProtocolOpts :: Type

foreign import protocolOpts :: List ProtoOpt -> ProtocolOpts

foreign import data ProtocolEnv :: Type
foreign import env :: List ProtoEnv -> ProtocolEnv
