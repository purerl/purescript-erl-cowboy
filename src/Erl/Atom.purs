module Erl.Atom where

foreign import data Atom :: *

foreign import atom :: String -> Atom
