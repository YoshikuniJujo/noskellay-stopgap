{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Mini.Database (
	E(..), fromSigned, toSigned,
	insert, selectAll1
	) where

import Language.Haskell.TH
import Data.Maybe
import Data.List qualified as L
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text qualified as T
import Data.Aeson qualified as A
import Nostr.Event.Signed qualified as Signed
import Nostr.Event.Json qualified as EvJsn
import Crypto.Curve.Secp256k1

import Event.Database.Tools
import Event.Mini.Database.TH
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
insert db ev' = withPrepared db (
		"INSERT INTO events VALUES(" ++
		L.intercalate ", "
			((: "") <$> replicate (length columns) '?') ++
		")" ) \sm ->
	$(doE $ binds 'sm 'ev' (zip [1 ..] columns) ++
		[noBindS $ varE 'step `appE` varE 'sm])

selectAll1 :: SQLite -> IO ((Result, Signed.E), String)
selectAll1 db = withPrepared db
	"SELECT * FROM events" \sm -> do
	r <- step sm

	idnt' <- column sm 0
	pk <- column sm 1
	crat <- column sm 2
	knd <- column sm 3
	a' <- column sm 4
	b' <- column sm 5
	c' <- column sm 6
	tgs <- column sm 7
	cnt <- column sm 8
	sg <- column sm 9
	vrf <- column sm 10

	let e = E {

		idnt = idnt',
		pubkey = pk,
		created_at = crat,
		kind = knd,
		a = a',
		b = b',
		c = c',
		tags = tgs,
		content = cnt,
		sig = sg,
		verified = vrf

		}
	pure (r, toSigned e)
