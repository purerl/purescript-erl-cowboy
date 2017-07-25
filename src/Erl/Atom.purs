module Erl.Atom where

foreign import data Atom :: Type

foreign import atom :: String -> Atom
