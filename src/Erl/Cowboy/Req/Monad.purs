module Erl.Cowboy.Req.Monad where

import Prelude
import Erl.Cowboy.Req as Req
import Control.Monad.State (State, get, modify, runState)
import Control.Monad.State.Class (class MonadState)
import Data.Tuple (Tuple(Tuple))
import Erl.Cowboy.Handler (Handler)
import Erl.Cowboy.Req (ok)
import Erl.Data.Tuple (tuple3)

reply :: forall m. (MonadState Req.Req m) => Req.StatusCode -> Req.Headers -> String -> m Unit
reply s h b = modify (Req.reply s h b)

replyWithoutBody :: forall m. (MonadState Req.Req m) => Req.StatusCode -> Req.Headers -> m Unit
replyWithoutBody s h = modify (Req.replyWithoutBody s h)

path :: forall m. (MonadState Req.Req m) => m String
path = Req.path <$> get

qs :: forall m. (MonadState Req.Req m) => m String
qs = Req.qs <$> get

-- | Construct a handler that does not use Cowboy's state parameter
handler :: forall a s. (State Req.Req s) -> Handler a
handler e req a =
  case runState e req of
    Tuple _ s -> tuple3 ok s a
