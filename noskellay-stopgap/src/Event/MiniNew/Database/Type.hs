{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.MiniNew.Database.Type where

data E = E {
	idnt :: String,
	pubkey :: String,
	created_at :: Int,
	kind :: Int,
	a :: [String],
	b :: [String],
	c :: [String],
	tags :: String,
	content :: String,
	sig :: String,
	verified :: Bool }
	deriving Show
