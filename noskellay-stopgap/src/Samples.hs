{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples (getSampleSigned, dataPart') where

import Control.Monad
import Data.Map qualified as Map
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UnixTime
import Codec.Binary.Bech32
import Nostr.Event qualified as Event
import Nostr.Event.Signed qualified as Signed

getSampleSigned :: FilePath -> FilePath -> IO Signed.E
getSampleSigned scfp pbfp = do
	Just sc <- Event.secretFromBech32 . chomp <$> T.readFile scfp
	Signed.signature sc =<< sample pbfp

chomp :: T.Text -> T.Text
chomp t = if T.last t == '\n' then T.init t else t

sample :: FilePath -> IO Event.E
sample fp = do
	Just pub <- dataPart . chomp <$> T.readFile fp
	Just pk <- pure $ Event.parse_point pub
	ut <- getUnixTime
	pure Event.E {
		Event.pubkey = pk,
		Event.created_at = ut,
		Event.kind = 1,
		Event.tags =
			Map.insert "A" ("bar", ["baz", "hoge"])
				$ Map.singleton "a" ("foo", ["bar", "baz"]),
		Event.content = "Hello" }

dataPart :: T.Text -> Maybe BS.ByteString
dataPart b = case decode b of
	Right (_, dataPartToBytes -> d) -> d
	_ -> error "bad"

dataPart' :: T.Text -> T.Text -> Maybe BS.ByteString
dataPart' tg0 b = case decode b of
	Right (humanReadablePartToText -> tg, dataPartToBytes -> d) ->
		guard (tg == tg0) *> d
	_ -> error "bad"
