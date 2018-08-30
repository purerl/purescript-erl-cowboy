module Erl.Cowboy.Handler where


import Control.Monad.State (StateT, evalStateT)
import Effect (Effect)
import Erl.Cowboy.Req (Req)

data Ok a = Ok Req a

type Handler a = Req -> a -> Ok a

type EffectHandler a = Req -> a -> Effect (Ok a)

runHandlerM :: forall res. HandlerM res -> Req -> Effect res
runHandlerM h req = evalStateT h req 

type HandlerM = StateT Req Effect
