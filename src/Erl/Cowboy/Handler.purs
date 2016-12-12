module Erl.Cowboy.Handler where

import Erl.Data.Tuple (Tuple3)
import Erl.Cowboy.Req (Ok, Req)

type Handler a = Req -> a -> Tuple3 Ok Req a
