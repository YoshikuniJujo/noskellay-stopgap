{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Data.Char
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Numeric

bsToHexStr :: BS.ByteString -> String
bsToHexStr = concat . (sh <$>) . map ord . BSC.unpack
	where
	sh n = let s = showHex n "" in replicate (2 - length s) '0' ++ s
