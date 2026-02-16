{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SamplesNG where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UnixTime
import Codec.Binary.Bech32
import "try-nostr-event-ng" Nostr.Event qualified as Event
import "try-nostr-event-ng" Nostr.Event.Signed qualified as Signed

getSample :: FilePath -> FilePath -> IO Signed.E
getSample scfp pbfp = do
	Just sc <- Event.secretFromBech32 . chomp <$> T.readFile scfp
	Signed.signature sc =<< sample pbfp

getSampleTags :: FilePath -> FilePath -> IO [(T.Text, (T.Text, [T.Text]))]
getSampleTags scfp pbfp = Signed.tags <$> getSample scfp pbfp

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
		Event.tags = [
			("a", ("foo", ["bar", "baz"])),
			("b", ("hoge", ["piyo", "huga"])),
			("A", ("bar", ["baz", "hoge"])),
			("a", ("oreore", ["oreda", "oreoda"]))
			],
		Event.content = "Hello" }

dataPart :: T.Text -> Maybe BS.ByteString
dataPart b = case decode b of
	Right (_, dataPartToBytes -> d) -> d
	_ -> error "bad"
