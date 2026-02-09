{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Mini.Database (
	E(..), fromSigned, toSigned,
	insert, count, selectAll, selectAll1
	) where

import Data.Function
import Data.Maybe
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text qualified as T
import Data.Aeson qualified as A
import Nostr.Event.Signed qualified as Signed
import Nostr.Event.Json qualified as EvJsn
import Crypto.Curve.Secp256k1

import Event.Database.Tools
import Event.Database.TH
import Event.Mini.Database.TH qualified as Mini
import Event.Mini.Database.Type
import Tools

import Database.SmplstSQLite3

fromSigned :: Signed.E -> E
fromSigned e = E {
	idnt = bsToHexStr $ Signed.id e,
	pubkey = bsToHexStr . BS.tail . serialize_point $ Signed.pubkey e,
	created_at = unixTimeToInt $ Signed.created_at e,
	kind = Signed.kind e,
	a = eventToTag e "a",
	b = eventToTag e "b",
	c = eventToTag e "c",
	tags = etgs e,
	content = T.unpack $ Signed.content e,
	sig = bsToHexStr $ Signed.sig e,
	verified = Signed.verified e }

toSigned :: E -> Signed.E
toSigned e = Signed.E {
	Signed.id = hexStrToBs $ idnt e,
	Signed.pubkey = fromJust . parse_point . hexStrToBs $ pubkey e,
	Signed.created_at = intToUnixTime $ created_at e,
	Signed.kind = kind e,
	Signed.tags = EvJsn.decodeTags . fromJust . A.decode . LBSC.pack $ tags e,
	Signed.content = T.pack $ content e,
	Signed.sig = hexStrToBs $ sig e,
	Signed.verified = verified e }

insert :: SQLite -> E -> IO (Result, String)
insert db ev' = withPrepared db (insertCommand Mini.columns) \sm ->
	$(mkInsert Mini.columns 'sm 'ev')

selectAll1 :: SQLite -> IO ((Result, Signed.E), String)
selectAll1 db = withPrepared db "SELECT * FROM events" \sm ->
	$(mkSelectAll1 Mini.columns 'sm 'E 'toSigned)

count :: SQLite -> IO (Int, String)
count db = withPrepared db "SELECT COUNT(*) FROM events" \sm -> do
	_ <- step sm
	column sm 0

selectAll :: SQLite -> IO ([Signed.E], String)
selectAll db = withPrepared db "SELECT * FROM events" \sm ->
	fix \go -> do
		r <- step sm
		case r of
			Done -> pure []
			Row -> do
				idnt' <- column sm 0
				pk <- column sm 1
				crat <- column sm 2
				knd <- column sm 3
				aa <- column sm 4
				bb <- column sm 5
				cc <- column sm 6
				tgs <- column sm 7
				cnt <- column sm 8
				sg <- column sm 9
				vrf <- column sm 10
				let e = E {
					idnt = idnt',
					pubkey = pk,
					created_at = crat,
					kind = knd,
					a = aa,
					b = bb,
					c = cc,
					tags = tgs,
					content = cnt,
					sig = sg,
					verified = vrf }
				(toSigned e :) <$> go
			_ -> error "bad"
