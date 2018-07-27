module Erl.Cowboy where

import Prelude

import Effect (Effect)
import Erl.Cowboy.Routes (Dispatch, Module)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple4)

foreign import startHttp :: Int -> List TransOpt -> List ProtoOpt -> Effect Unit

data TransOpt = Ip (Tuple4 Int Int Int Int) | Port Int
data ProtoOpt = Env (List ProtoEnv) | Middlewares (List Module)
data ProtoEnv = Dispatch Dispatch | Fn (Effect Unit)
