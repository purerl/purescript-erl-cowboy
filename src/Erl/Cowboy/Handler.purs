module Erl.Cowboy.Handler where


import Prelude

import Control.Monad.State (StateT, runStateT)
import Data.Tuple (uncurry)
import Effect (Effect)
import Erl.Cowboy.Req (Req)

type HandlerM = StateT Req Effect

runHandlerM :: forall a b. (a -> Req -> b) -> HandlerM a -> Req -> Effect b
runHandlerM res h req = uncurry res <$> runStateT h req
