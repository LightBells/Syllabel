module Util.BSUtils where

import qualified Data.ByteString as BS
import Codec.Binary.UTF8.String

decodeByteString = decode . BS.unpack
