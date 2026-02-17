{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Mini.Database where

import Control.Monad
import Data.Map qualified as Map
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text qualified as T
import Data.Aeson qualified as A
import Data.UUIDv7
import Crypto.Curve.Secp256k1
import "try-nostr-event-ng" Nostr.Event.Signed qualified as Signed
import "try-nostr-event-ng" Nostr.Event.Json qualified as EvJsn
import Event.NG.Mini.Database.Type

import Event.Database.Tools
import Tools

fromSigned :: Signed.E -> IO (E, [Tag])
fromSigned e = do
	dct <- genId
	pure (fromSignedE dct e, tagsToTags dct $ Signed.tags e)

genId :: IO (Map.Map T.Text UUIDv7)
genId = Map.fromList
	<$> zipWith ((,) . (T.:< "")) "abc" <$> replicateM 3 nextUUIDv7

fromSignedE :: Map.Map T.Text UUIDv7 -> Signed.E -> E
fromSignedE dct e = E {
	idnt = bsToHexStr $ Signed.idnt e,
	pubkey = bsToHexStr . BS.tail . serialize_point $ Signed.pubkey e,
	created_at = unixTimeToInt $ Signed.created_at e,
	kind = Signed.kind e,
	a = dct Map.! "a",
	b = dct Map.! "b",
	c = dct Map.! "c",
	tags = LBSC.unpack . A.encode . EvJsn.encodeTags $ Signed.tags e,
	content = T.unpack $ Signed.content e,
	sig = bsToHexStr $ Signed.sig e,
	verified = Signed.verified e }

tagsToTags :: Map.Map T.Text UUIDv7 -> [(T.Text, (T.Text, [T.Text]))] -> [Tag]
tagsToTags dct = \case
	[] -> []
	(k, (v, _)) : kvs -> case dct Map.!? k of
		Nothing -> tagsToTags dct kvs
		Just u -> Tag u (T.unpack v) : tagsToTags dct kvs
