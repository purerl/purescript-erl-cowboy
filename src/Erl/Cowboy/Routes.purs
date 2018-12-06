-- | Bindings for cowboy_router
module Erl.Cowboy.Routes where

import Prelude

import Erl.Atom (atom)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2, Tuple3, tuple2, tuple3)
import Erl.ModuleName (NativeModuleName)
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)

-- | Initial state of a route. Different routes will have different underlying state types, this is *not* safe and is not
-- | linked to the actual module state type :(
newtype InitialState = InitialState Foreign

newtype Route = Route (Tuple2 MatchSpec (List Path))
newtype Path = Path (Tuple3 MatchSpec NativeModuleName InitialState)

-- | Match spec for host or route (string host/path or placeholder _)
foreign import data MatchSpec :: Type

anyMatchSpec :: MatchSpec
anyMatchSpec = unsafeCoerce $ atom "_"

matchSpec :: String -> MatchSpec
matchSpec = unsafeCoerce 

foreign import data Dispatch :: Type
foreign import compile :: List Route -> Dispatch

anyHost :: List Path -> Route
anyHost = Route <<< tuple2 anyMatchSpec

host :: String -> List Path -> Route
host h = Route <<< tuple2 (matchSpec h)

anyPath :: NativeModuleName -> InitialState -> Path
anyPath m s = Path $ tuple3 anyMatchSpec m s

path :: String -> NativeModuleName -> InitialState -> Path
path spec m s = Path $ tuple3 (matchSpec spec) m s