module Attribute where

data Attribute (name :: Symbol) (content :: Symbol) = Attribute

type Behaviour = Attribute "behaviour"
