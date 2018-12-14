module Erl.Cowboy.Handlers.Rest

where

import Attribute (Attribute(..), Behaviour)
import Effect.Uncurried (EffectFn2, EffectFn3)
import Erl.Atom (Atom)
import Erl.Cowboy.Handlers.Common as C
import Erl.Cowboy.Req (Req)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2)
import Erl.ModuleName (NativeModuleName(..))

foreign import data InitResult :: Type -> Type

foreign import initResult :: forall a. a -> Req -> InitResult a 

type InitHandler c s = EffectFn2 Req c (InitResult s)

type TerminateHandler s = EffectFn3 C.TerminateReason Req s C.TerminateResult

-- | RestResult r s is the result of a rest callback with result r and state s
foreign import data RestResult :: Type -> Type -> Type

foreign import restResult :: forall r s. r -> s -> Req -> RestResult r s

foreign import stop :: forall r s. s -> Req -> RestResult r s

foreign import switchHandler :: forall r s. NativeModuleName -> s -> Req -> RestResult r s

type RestHandler r s = EffectFn2 Req s (RestResult r s)

-- | Handler for allowed_methods callback
type AllowedMethodsHandler s = RestHandler (List String) s

-- | Handler for allow_missing_post callback
type AllowedMissingPostHandler s = RestHandler Boolean s

-- | Handler for charsets_provided callback
type CharsetsProvidedHandler s = RestHandler (List String) s

data ContentType = ContentType String String ContentTypeParams | SimpleContentType String

data ContentTypeParams = AnyParams | ContentTypeParams (List (Tuple2 String String))

foreign import data ContentTypesAcceptedResult :: Type

-- foreign import data AcceptCallbackResult :: Type -> Type

-- type AcceptCallback s = EffectFn2 Req s (AcceptCallbackResult s)

newtype AcceptCallback = AcceptCallback Atom

foreign import contentTypesAcceptedResult :: List (Tuple2 ContentType AcceptCallback) -> ContentTypesAcceptedResult 

-- | Handler for content_types_accepted callback
type ContentTypesAcceptedHandler s = RestHandler ContentTypesAcceptedResult s

foreign import data ContentTypesProvidedResult :: Type

-- foreign import data ProvideCallbackResult :: Type -> Type

-- type ProvideCallback s = EffectFn2 Req s (ProvideCallbackResult s)

newtype ProvideCallback = ProvideCallback Atom

foreign import contentTypesProvidedResult :: List (Tuple2 ContentType ProvideCallback) -> ContentTypesProvidedResult 

-- | Handler for content_types_provided callback
type ContentTypesProvidedHandler s = RestHandler ContentTypesProvidedResult s

-- | Handler for delete_completed callback
type DeleteCompletedHandler s = RestHandler Boolean s

-- | Handler for delete_resource callback
type DeleteResourceHandler s = RestHandler Boolean s

-- TODO: Representation of calendar:datetime()
-- | Handler for expires callback
-- type ExpiresCallback = RestHandler 

-- | Handler for forbidden callback
type ForbiddenHandler s = RestHandler Boolean s

-- | Strong or weak etag
data ETag = Strong String | Weak String

-- | Handler for generate_etag callback
type GenerateEtagHandler s = RestHandler ETag s

foreign import data IsAuthorizedResponse :: Type

foreign import authorized :: IsAuthorizedResponse
foreign import unauthorized :: String -> IsAuthorizedResponse

-- | Handler for is_authorized callback
type IsAuthorizedHandler s = RestHandler IsAuthorizedResponse s

-- | Handler for _ callback
type IsConflictHandler s = RestHandler Boolean s

-- | Handler for _ callback
type KnownMethodsHandler s = RestHandler (List String) s

-- | Handler for _ callback
type LanguagesProvidedHandler s = RestHandler (List String) s

-- TODO date
-- | Handler for _ callback
-- type LastModifiedHandler s = RestHandler ?

-- | Handler for _ callback
type MalformedRequestHandler s = RestHandler Boolean s


foreign import data MovedResult :: Type

foreign import notMoved :: MovedResult

foreign import moved :: String -> MovedResult

-- | Handler for moved_permanently callback
type MovedPermanentlyHandler s = RestHandler MovedResult s

-- | Handler for moved_temporarily callback
type MovedTemporarilyHandler s = RestHandler MovedResult s

-- | Handler for multiple_choices callback
type MultipleChoicesHandler s = RestHandler Boolean s

foreign import data OptionsResponse :: Type

foreign import optionsResponse :: OptionsResponse

-- | Handler for options callback
type OptionsHandler s = RestHandler OptionsResponse s

-- | Handler for previously_existed callback
type PreviouslyExistedHandler s = RestHandler Boolean s

-- | Handler for resource_exists callback
type ResourceExistsHandler s = RestHandler Boolean s

-- | Handler for service_available callback
type ServiceAvailableHandler s = RestHandler Boolean s

-- | Handler for uri_too_long callback
type UriTooLongHandler s = RestHandler Boolean s

-- | Handler for valid_content_headers callback
type ValidContentHeadersHandler s = RestHandler Boolean s

-- | Handler for valid_entity_length callback
type ValidEntityLengthHandler s = RestHandler Boolean s

-- | Handler for variances callback
type VariancesHandler s = RestHandler (List String) s

type CowboyRestBehaviour = Behaviour "cowboy_rest"

-- | A cowboy_rest behaviour. Note that while may callbacks are defined only init is mandatory
cowboyRestBehaviour :: forall a s.
  { init :: InitHandler a s
  } -> CowboyRestBehaviour
cowboyRestBehaviour _ = Attribute
