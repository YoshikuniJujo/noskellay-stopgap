{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Database.Common where

import Data.Map qualified as Map
import Data.Int
import Data.Char
import Data.Text qualified as T
import Data.UUIDv7

tagsToTags :: Map.Map T.Text UUIDv7 -> [(T.Text, (T.Text, [T.Text]))] -> [(T.Text, Tag)]
tagsToTags dct = \case
	[] -> []
	(checkUpper -> k, (v, _)) : kvs -> case dct Map.!? k of
		Nothing -> tagsToTags dct kvs
		Just u -> (k, Tag (fst u') (snd u') (T.unpack v)) : tagsToTags dct kvs
			where u' = toInts u

checkUpper :: T.Text -> T.Text
checkUpper = \case c T.:< ""	| isUpper c -> "u" T.:> toLower c; c -> c

data Tag = Tag {
	uuidH :: Int64,
	uuidL :: Int64,
	tagValue :: String }
	deriving Show
