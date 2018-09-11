-- | Types and helpers for a cowboy_handler "Plain HTTP handler" callback module
module Erl.Cowboy.Handlers.Simple (
  InitResult
, InitHandler
, initResult
, TerminateHandler
, module C
, CowboyHandlerBehaviour
, cowboyHandlerBehaviour
) where

import Attribute (Attribute(..), Behaviour)
import Effect.Uncurried (EffectFn2, EffectFn3)
import Erl.Cowboy.Handlers.Common (CrashType(..), RawReason, TerminateReason(..), TerminateResult, decodeReason, terminateResult) as C
import Erl.Cowboy.Req (Req)

foreign import data InitResult :: Type -> Type

foreign import initResult :: forall a. a -> Req -> InitResult a

type InitHandler c s = EffectFn2 Req c (InitResult s)

type TerminateHandler s = EffectFn3 C.TerminateReason Req s C.TerminateResult

type CowboyHandlerBehaviour = Behaviour "cowboy_handler"

-- | A simple cowboy_handler behaviour. A terminate callback is optional.
cowboyHandlerBehaviour :: forall a s. { init :: InitHandler a s } -> CowboyHandlerBehaviour
cowboyHandlerBehaviour _ = Attribute