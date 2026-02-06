{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Mini.Database (
	E(..), fromSigned, toSigned,
	insert, selectAll1
	) where

import Data.Maybe
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text qualified as T
import Data.Aeson qualified as A
import Nostr.Event.Signed qualified as Signed
import Nostr.Event.Json qualified as EvJsn
import Crypto.Curve.Secp256k1

import Event.Database.Tools
import Tools

import Database.SmplstSQLite3

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

-- TRY IT
-- ghci> ev <- getSampleSigned "/path/to/sec_file" "/path/to/pub_file"
-- ghci> Event.Mini.Database.fromSigned ev

insert :: SQLite -> E -> IO (Result, String)
insert db ev' = withPrepared db
	"INSERT INTO events VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" \sm -> do
	bindN sm 1 $ idnt ev'
	bindN sm 2 $ pubkey ev'
	bindN sm 3 $ created_at ev'
	bindN sm 4 $ kind ev'
	bindN sm 5 $ a ev'
	bindN sm 6 $ b ev'
	bindN sm 7 $ c ev'
	bindN sm 8 $ tags ev'
	bindN sm 9 $ content ev'
	bindN sm 10 $ sig ev'
	bindN sm 11 $ verified ev'
	step sm

selectAll1 :: SQLite -> IO ((Result, Signed.E), String)
selectAll1 db = withPrepared db
	"SELECT * FROM events" \sm -> do
	r <- step sm
	(idnt' :: String) <- column sm 0
	(pk :: String) <- column sm 1
	(crat :: Int) <- column sm 2
	(knd :: Int) <- column sm 3
	(a' :: Maybe String) <- column sm 4
	(b' :: Maybe String) <- column sm 5
	(c' :: Maybe String) <- column sm 6
	tgs <- column sm 7
	(cnt :: String) <- column sm 8
	(sg :: String) <- column sm 9
	(vrf :: Bool) <- column sm 10
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
		verified = vrf }
	pure (r, toSigned e)
