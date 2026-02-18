{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Mini.Database.Type where

import Data.Int
import Data.ByteString qualified as BS
import Data.UUIDv7

data E = E {
	idnt :: BS.ByteString,
	pubkey :: BS.ByteString,
	created_at :: Int,
	kind :: Int,
	ah :: Int64,
	al :: Int64,
	bh :: Int64,
	bl :: Int64,
	ch :: Int64,
	cl :: Int64,
	tags :: String,
	content :: String,
	sig :: BS.ByteString,
	verified :: Bool }
	deriving Show

data Tag = Tag {
	uuidH :: Int64,
	uuidL :: Int64,
	tagValue :: String }
	deriving Show
