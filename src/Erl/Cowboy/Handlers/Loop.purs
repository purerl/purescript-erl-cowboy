-- | Types and handlers for a cowboy_loop loop handler callback module.
module Erl.Cowboy.Handlers.Loop (
  InitResult
, InitHandler
, initResult
, hibernate
, InfoResult
, InfoHandler
, continue
, continueHibernate
, stop
, module C
) where

import Effect.Uncurried (EffectFn2, EffectFn3)
import Erl.Cowboy.Req (Req)
import Erl.Cowboy.Handlers.Common (CrashType(..), RawReason, TerminateReason(..), TerminateResult, decodeReason, terminateResult) as C

foreign import data InitResult :: Type -> Type

foreign import initResult :: forall a. a -> Req -> InitResult a
foreign import hibernate :: forall a. a -> Req -> InitResult a

type InitHandler c s = EffectFn2 Req c (InitResult s)

foreign import data InfoResult :: Type -> Type

-- | ok response
foreign import continue :: forall a. a -> Req -> InfoResult a 

-- | ok, hibernate response
foreign import continueHibernate :: forall a. a -> Req -> InfoResult a 

-- | stop response
foreign import stop :: forall a. a -> Req -> InfoResult a 

type InfoHandler a s = EffectFn3 a Req s (InfoResult s)

type TerminateHandler s = EffectFn3 C.TerminateReason Req s C.TerminateResult