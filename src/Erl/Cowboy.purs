module Erl.Cowboy where

import Prelude
import Erl.Data.List
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Erl.Atom (Atom, atom)
import Erl.Cowboy.Req (Ok)
import Erl.Data.Tuple (Tuple2, Tuple3, Tuple4, tuple2)

type Routes a = List (Tuple2 Atom (Paths a))
type Paths a = List (Tuple3 CharString Module a)
type Module = Atom

foreign import data PROCESS :: !
foreign import data Pid :: *

foreign import startHttp :: forall eff. Int -> List TransOpt -> List ProtoOpt -> Eff (process :: PROCESS | eff) Unit

foreign import data Dispatch :: *
foreign import compile :: forall a. Routes a -> Dispatch


data TransOpt = Ip (Tuple4 Int Int Int Int) | Port Int
data ProtoOpt = Env (List ProtoEnv) | Middlewares (List Module)
type ProtoEnv = Tuple2 Atom EnvValue -- Dispatch Dispatch | Fn (Eff (console :: CONSOLE) Unit)

foreign import data EnvValue :: *

foreign import toEnvValue :: forall a. a -> EnvValue

dispatch :: Dispatch -> ProtoEnv
dispatch = tuple2 (atom "dispatch") <<< toEnvValue

foreign import data CharString :: *
foreign import string :: String -> CharString
