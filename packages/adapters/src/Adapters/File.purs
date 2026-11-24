-- | One text file on disk: read it when it exists, and replace it whole (written beside it and renamed over it, so a
-- | crash leaves the old contents or the new, never half of either).
module Adapters.File (textFile) where

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Prelude (Unit, (<$>))

foreign import readImpl :: String -> Effect (Nullable String)
foreign import writeImpl :: String -> String -> Effect Unit

textFile :: String -> { read :: Effect (Maybe String), write :: String -> Effect Unit }
textFile path = { read: toMaybe <$> readImpl path, write: writeImpl path }
