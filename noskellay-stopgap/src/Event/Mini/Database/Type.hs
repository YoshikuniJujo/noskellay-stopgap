{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Mini.Database.Type where

data E = E {
	idnt :: String,
	pubkey :: String,
	created_at :: Int,
	kind :: Int,
	a :: Maybe String,
	b :: Maybe String,
	c :: Maybe String,
	tags :: String,
	content :: String,
	sig :: String,
	verified :: Bool }
	deriving Show
