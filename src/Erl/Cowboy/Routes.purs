module Erl.Cowboy.Routes where

import Prelude

import Erl.Atom (Atom, atom)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2, Tuple3)

type Routes a = List (Tuple2 Atom (Paths a))
type Paths a = List (Tuple3 String Module a)
newtype Module = Module Atom

mod :: String -> Module
mod = Module <<< atom

foreign import data Dispatch :: Type
foreign import compile :: forall a. Routes a -> Dispatch
