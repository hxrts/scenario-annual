-- | The `DESK` interpreter: the held issue in a reference, private to the handler.
module Handlers.Desk (Desk, newDesk, interpretDesk) where

import Prelude

import Edition.Issue (Issue(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effects.Signatures (DeskF(..))

newtype Desk = Desk (Ref Issue)

-- | An empty desk holds no run until the first refresh.
newDesk :: Effect Desk
newDesk = Desk <$> Ref.new NoRun

interpretDesk :: forall m. MonadEffect m => Desk -> DeskF ~> m
interpretDesk (Desk ref) = liftEffect <<< case _ of
  Current k -> k <$> Ref.read ref
  Publish issue next -> next <$ Ref.write issue ref
