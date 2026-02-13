{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Mini.Database.Type where

import Data.UUID

data E = E {
	idnt :: String,
	pubkey :: String,
	created_at :: Int,
	kind :: Int,
	a :: UUID,
	b :: UUID,
	c :: UUID,
	tags :: String,
	content :: String,
	sig :: String,
	verified :: Bool }
	deriving Show

data Tag = Tag {
	tagKey :: UUID,
	tagValue :: String }
	deriving Show
