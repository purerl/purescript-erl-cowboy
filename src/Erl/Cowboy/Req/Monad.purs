module Erl.Cowboy.Req.Monad
( reply
, replyWithoutBody
, replyWithFile
, replyStatus
, streamReply
, streamBody
, streamBodyFinal
, path
, qs
) where

import Prelude

import Control.Monad.State (get, put)
import Control.Monad.State.Class (class MonadState)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Cowboy.Req as Req
import Erl.Data.Binary (Binary)

-- Like modify_ but with an effectful modification
modifyEffect_ :: forall s m. MonadState s m => MonadEffect m => (s -> Effect s) -> m Unit
modifyEffect_ f = get >>= (liftEffect <<< f) >>= put

reply :: forall m. MonadState Req.Req m => MonadEffect m  => Req.StatusCode -> Req.Headers -> String -> m Unit
reply s h b = modifyEffect_ (Req.reply s h b)

replyWithoutBody :: forall m. MonadState Req.Req m => MonadEffect m => Req.StatusCode -> Req.Headers -> m Unit
replyWithoutBody s h = modifyEffect_ (Req.replyWithoutBody s h)

replyWithFile :: forall m. MonadState Req.Req m => MonadEffect m => Req.StatusCode -> Req.Headers -> String -> m Unit
replyWithFile s h f = modifyEffect_ (Req.replyWithFile s h f)

replyStatus :: forall m. MonadState Req.Req m => MonadEffect m => Req.StatusCode -> m Unit
replyStatus s = modifyEffect_ (Req.replyStatus s)

streamReply :: forall m. MonadState Req.Req m => MonadEffect m  => Req.StatusCode -> Req.Headers -> m Unit
streamReply s h = modifyEffect_ (Req.streamReply s h)

streamBody :: forall m. MonadState Req.Req m => MonadEffect m  => Binary -> m Unit
streamBody b = get >>= (liftEffect <<< Req.streamBody b)

streamBodyFinal :: forall m. MonadState Req.Req m => MonadEffect m  => Binary -> m Unit
streamBodyFinal b = get >>= (liftEffect <<< Req.streamBodyFinal b)

path :: forall m. (MonadState Req.Req m) => m String
path = Req.path <$> get

qs :: forall m. (MonadState Req.Req m) => m String
qs = Req.qs <$> get

