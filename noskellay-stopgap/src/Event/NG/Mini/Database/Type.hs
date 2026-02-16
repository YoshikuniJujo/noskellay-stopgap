{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Mini.Database.Type where

import Data.UUIDv7

data E = E {
	idnt :: String,
	pubkey :: String,
	created_at :: Int,
	kind :: Int,
	a :: UUIDv7,
	b :: UUIDv7,
	c :: UUIDv7,
	tags :: String,
	content :: String,
	sig :: String,
	verified :: Bool }
	deriving Show

data Tag = Tag {
	tagKey :: UUIDv7,
	tagValue :: String }
	deriving Show
