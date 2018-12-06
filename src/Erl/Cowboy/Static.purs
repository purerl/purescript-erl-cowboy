-- | Convenience binding to cowboy_static
module Cowboy.Static where

import Prelude

import Erl.Atom (Atom, atom)
import Erl.Cowboy.Routes (InitialState(..), Path, path)
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName as ModuleName
import Foreign (unsafeToForeign)

moduleName :: ModuleName.NativeModuleName
moduleName = ModuleName.NativeModuleName (atom "cowboy_static")

privFile :: Atom -> String -> String -> Path
privFile app url f = path url moduleName (InitialState $ unsafeToForeign $ tuple3 (atom "priv_file") app f)

file :: String -> String -> Path
file url f = path url moduleName (InitialState $ unsafeToForeign $ tuple2 (atom "file") f)

privDir :: Atom -> String -> String -> Path
privDir app url d = path url moduleName (InitialState $ unsafeToForeign $ tuple3 (atom "priv_dir") app d)

dir :: String -> String -> Path
dir url d = path url moduleName (InitialState $ unsafeToForeign $ tuple2 (atom "dir") d)
