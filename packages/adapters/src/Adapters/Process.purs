-- | The process: its environment, a line to standard output, and a repeating timer.
module Adapters.Process (env, writeLine, every) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

foreign import envImpl :: String -> Effect (Nullable String)

-- | One environment variable, when set.
env :: String -> Effect (Maybe String)
env name = toMaybe <$> envImpl name

-- | A line to standard output, stamped with the UTC time.
foreign import writeLine :: String -> Effect Unit

foreign import everyImpl :: Fn2 Int (Effect Unit) (Effect Unit)

-- | Run an action every so many milliseconds, the first time after one interval.
every :: Int -> Effect Unit -> Effect Unit
every = runFn2 everyImpl
