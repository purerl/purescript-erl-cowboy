module Erl.Cowboy where

import Prelude

import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2, Tuple3, Tuple4, tuple2)

type Routes a = List (Tuple2 Atom (Paths a))
type Paths a = List (Tuple3 CharString Module a)
type Module = Atom

foreign import data Pid :: Type

foreign import startHttp :: Int -> List TransOpt -> List ProtoOpt -> Effect Unit

foreign import data Dispatch :: Type
foreign import compile :: forall a. Routes a -> Dispatch

data TransOpt = Ip (Tuple4 Int Int Int Int) | Port Int
data ProtoOpt = Env (List ProtoEnv) | Middlewares (List Module)
type ProtoEnv = Tuple2 Atom EnvValue

foreign import data EnvValue :: Type

foreign import toEnvValue :: forall a. a -> EnvValue

dispatch :: Dispatch -> ProtoEnv
dispatch = tuple2 (atom "dispatch") <<< toEnvValue

foreign import data CharString :: Type
foreign import string :: String -> CharString
