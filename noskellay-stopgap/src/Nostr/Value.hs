{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Nostr.Value where

import Data.Int
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Database.SmplstSQLite3

data Value
	= VText T.Text
	| VInt Int64
	| VBlob BS.ByteString
	deriving Show

instance Bindable Value where
	bindN' sm n (VText t) = bindN' sm n t
	bindN' sm n (VInt i) = bindN' sm n i
	bindN' sm n (VBlob b) = bindN' sm n b
