-- | Bindings for cowboy_req
-- |
-- | The `Req` type (corresponding to `req()`) is core to the use of cowboy, containing information about request and response.
-- | Many functions operating on `Req` are pure, producing an updated `Req` if required to update some fields, e.g. when setting a
-- | header, but others produce a result in `Effect` as even though they may update the `Req`, they also cause side-effects such as
-- | sending traffic on the network (eg `reply`).
module Erl.Cowboy.Req 
  ( StatusCode(..)
  , Headers
  , Req
  , reply
  , replyWithoutBody
  , replyWithFile
  , replyStatus
  , method
  , Version(..)
  , version
  , scheme
  , binding
  , bindingWithDefault
  , pathInfo
  , host
  , port
  , path
  , qs
  , header
  , headers
  , ReadBodyResult(..)
  , readBody
  , setHeader
  , setBody
  , IpAddress
  , peer
  , streamReply
  , streamBody
  , streamBodyFinal
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.Binary (Binary)
import Erl.Data.List (List)
import Erl.Data.Map (Map)
import Erl.Data.Tuple (Tuple2, Tuple4)

foreign import data Req :: Type

-- http_status() = non_neg_integer() | binary()
newtype StatusCode = StatusCode Int

type Headers = Map String String

-- | Send the reply including the given body content (cowboy_req:reply/4)
foreign import reply :: StatusCode -> Headers -> String -> Req -> Effect Req

-- | Send the reply without setting the body (cowboy_req:reply/3)
foreign import replyWithoutBody :: StatusCode -> Headers -> Req -> Effect Req

-- | Send the reply including a file as the body
foreign import replyWithFile :: StatusCode -> Headers -> String -> Req -> Effect Req

-- | Send the reply with already set headers and body (cowboy_req:reply/2)
foreign import replyStatus :: StatusCode -> Req -> Effect Req

-- Raw request

foreign import method :: Req -> String

data Version = HTTP1_0 | HTTP1_1 | HTTP2

foreign import versionImpl :: Version -> Version -> Version -> Req -> Version

version :: Req -> Version
version = versionImpl HTTP1_0 HTTP1_1 HTTP2

foreign import scheme :: Req -> String

foreign import bindingWithDefault :: forall a. Atom -> Req -> a -> a

foreign import bindingImpl :: forall a. (Maybe a) -> (a -> Maybe a) -> Atom -> Req -> Maybe a

binding :: forall a. Atom -> Req -> Maybe a
binding = bindingImpl Nothing Just 

foreign import pathInfo :: Req -> List String

foreign import host :: Req -> String

foreign import port :: Req -> Int

foreign import path :: Req -> String

foreign import qs :: Req -> String

-- cowboy_req:uri(3) - Reconstructed URI

foreign import headerImpl :: (forall a. Maybe a) -> (forall a. a -> Maybe a) -> String -> Req -> Maybe String

header :: String -> Req -> Maybe String
header = headerImpl Nothing Just

foreign import headers :: Req -> Headers

-- Reading the body

data ReadBodyResult = FullData Binary Req | PartialData Binary Req

foreign import readBodyImpl :: (Binary -> Req -> ReadBodyResult) -> (Binary -> Req -> ReadBodyResult) -> Req -> Effect ReadBodyResult

readBody :: Req -> Effect ReadBodyResult
readBody = readBodyImpl FullData PartialData

-- Writing a response

foreign import setHeader :: String -> String -> Req -> Req

foreign import setCookie :: String -> String -> Req -> Req

-- | Set response body. As should be apparent from the type, this does not actually send the body but merely sets it in the Req, 
-- | the body is sent once reply is called.
foreign import setBody :: String -> Req -> Req

type IpAddress = Tuple4 Int Int Int Int

foreign import peer :: Req -> Tuple2 IpAddress Int

-- Streaming responses

foreign import streamReply :: StatusCode -> Headers -> Req -> Effect Req

-- TODO: binary/iolist ?
foreign import streamBody :: Binary -> Req -> Effect Unit

foreign import streamBodyFinal :: Binary -> Req -> Effect Unit
