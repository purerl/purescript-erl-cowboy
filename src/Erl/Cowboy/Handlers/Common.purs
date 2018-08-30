-- | Types/conversions shared between several callback handler behaviours
module Erl.Cowboy.Handlers.Common 
  ( CrashType(..)
  , TerminateReason(..)
  , decodeReason
  , RawReason
  , TerminateResult
  , terminateResult
  )
where

data CrashType = Error | Exit | Throw

-- | Reason for a crash. The Reason :: any() is currently discarded
data TerminateReason = Normal | Crash CrashType

foreign import data RawReason :: Type

foreign import decodeReasonImpl :: TerminateReason -> (CrashType -> TerminateReason) -> CrashType -> CrashType -> CrashType -> RawReason -> TerminateReason

decodeReason :: RawReason -> TerminateReason
decodeReason = decodeReasonImpl Normal Crash Error Exit Throw

foreign import data TerminateResult :: Type

foreign import terminateResult :: TerminateResult 