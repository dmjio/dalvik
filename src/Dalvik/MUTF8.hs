-- | A MUTF8 decoder for Dalvik strings
--
-- Dalvik strings are stored in MUTF8 format - a modified UTF-8.  The
-- main difference is that code points requiring more than three bytes
-- are encoded using surrogate pairs.  See
-- <http://source.android.com/devices/tech/dalvik/dex-format.html> for
-- details.
module Dalvik.MUTF8 ( decodeMUTF8 ) where

import Data.ByteString ( ByteString )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

-- | Make a best effort to decode MUTF-8 strings safely.
--
-- MUTF-8 is close to UTF-8 for most common strings, so just try that
-- and replace anything invalid with the unicode replacement
-- character.  This will do until we write a real MUTF-8 decoder.
decodeMUTF8 :: ByteString -> String
decodeMUTF8 = T.unpack . T.decodeUtf8With T.lenientDecode
