{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Mini.Database where

import Control.Monad
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.UUIDv7
import "try-nostr-event-ng" Nostr.Event.Signed qualified as Signed
import Event.NG.Mini.Database.Type

fromSigned :: Signed.E -> IO (E, [Tag])
fromSigned e = undefined

genId :: IO (Map.Map T.Text UUIDv7)
genId = Map.fromList
	<$> zipWith ((,) . (T.:< "")) "abc" <$> replicateM 3 nextUUIDv7

tagsToTags :: Map.Map T.Text UUIDv7 -> [(T.Text, (T.Text, [T.Text]))] -> [Tag]
tagsToTags dct = \case
	[] -> []
	(k, (v, _)) : kvs -> case dct Map.!? k of
		Nothing -> tagsToTags dct kvs
		Just u -> Tag u (T.unpack v) : tagsToTags dct kvs
