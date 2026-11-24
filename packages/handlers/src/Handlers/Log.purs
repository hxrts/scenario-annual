-- | The `LOG` interpreter over a line sink.
module Handlers.Log (LineSink, interpretLog) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effects.Signatures (LogF(..))

type LineSink = String -> Effect Unit

interpretLog :: forall m. MonadEffect m => LineSink -> LogF ~> m
interpretLog sink (Note line next) = next <$ liftEffect (sink line)
