-- | A short fingerprint of a text: the same text always gives the same fingerprint, so it can name a version of the
-- | content (an HTTP entity tag) across restarts. Not cryptographic: it tells versions apart, it does not guard them.
module Edition.Digest (digest) where

import Prelude

import Data.Array (foldl)
import Data.Int (hexadecimal, toStringAs)
import Data.Int.Bits (xor, zshr)
import Data.String.CodeUnits (length, toCharArray)
import Data.Char (toCharCode)

-- | Two 32-bit djb2-style hashes (multiply by 33 and add; multiply by 31 and xor), with the length.
digest :: String -> String
digest text = toStringAs hexadecimal (length text) <> "-" <> hex a <> hex b
  where
  codes = map toCharCode (toCharArray text)
  a = foldl (\h c -> h * 33 + c) 5381 codes
  b = foldl (\h c -> (h * 31) `xor` c) 52711 codes
  hex n = toStringAs hexadecimal (n `zshr` 0)
