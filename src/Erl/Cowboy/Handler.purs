-- | Cowboy handlers.
-- |
-- | Submodules of `Erl.Cowboy.Handler` each present a number of type abbreviations which match the required callback methods for a cowboy behaviour/callback module,
-- | along with bindings to construct/destruct native cowboy types where appropriate.
-- | 
-- | To correctly define a cowboy callback module the correct named bindings must be present at the top level with the appropriate types, if the
-- | `EffectFnX` based types in the handler modules are used along with correctly named top level bindings this should all work out, due to the way the
-- | purerl compiler generates code for uncurried function overloads of `EffectFnX` and `FnX` types. A no-op behaviour
-- | function is provided in each handler module to collect the mandatory callbacks with the correct names and types, *if* this is filled in with direct 
-- | reference to top-level bindings, "this should all work out". It cannot be stressed enough this is all "assistive" optional make-believe, the value-level 
-- | implementation of `whateverBehaviour` is a constant which will be ignored, all that matters is that you define values at the top level with the right names
-- | and types that ensure they get compiled to Erlang functions of the correct arity and behaviour.
-- | 
-- | An example for `Erl.Cowboy.Handlers.WebSocket`:
-- |
-- | ```purescript
-- | _behaviour :: CowboyWebsocketBehaviour
-- | _behaviour = cowboyWebsocketBehaviour { init, websocket_handle, websocket_info }
-- | 
-- | init :: InitHandler Config HandlerState
-- | init = ...
-- | ```
-- |
-- | This module also contains utilities to assist in defing handlers using a the state monad transfomer. See `Erl.Cowboy.Req.Monad` for related convenience wrappers
-- | for working with request objects inside a state monad.
module Erl.Cowboy.Handler where

import Prelude

import Control.Monad.State (StateT, runStateT)
import Data.Tuple (uncurry)
import Effect (Effect)
import Erl.Cowboy.Req (Req)

type HandlerM = StateT Req Effect

runHandlerM :: forall a b. (a -> Req -> b) -> HandlerM a -> Req -> Effect b
runHandlerM res h req = uncurry res <$> runStateT h req
