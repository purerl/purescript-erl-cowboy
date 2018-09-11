-- | Representation of erlang attributes with symbols and compiler magic.
module Attribute where

-- | An attribute with specified name and content
data Attribute (name :: Symbol) (content :: Symbol) = Attribute

-- | A behaviour attribute, e.g. ```Behaviour "gen_server"```
type Behaviour = Attribute "behaviour"
