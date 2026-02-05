{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Database where

import Prelude hiding (id)
import Foreign.C.Types
import Control.Arrow
import Data.Maybe
import Data.Map qualified as Map
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text qualified as T
import Data.Aeson qualified as A
import Data.UnixTime
import Language.Haskell.TH
import ToolsTH
import Nostr.Event.Signed qualified as Signed
import Nostr.Event.Json qualified as EvJsn
import Crypto.Curve.Secp256k1

import Tools

(: []) <$> dataD (cxt []) (mkName "Foo") [] Nothing [
	recC (mkName "Foo") ([
		varBangType (mkName "idnt")
			$ bangType noUnpackedNoStrict (conT ''String),
		varBangType (mkName "pubkey")
			$ bangType noUnpackedNoStrict (conT ''String),
		varBangType (mkName "created_at")
			$ bangType noUnpackedNoStrict (conT ''Int) ] ++ [
		varBangType (mkName "kind")
			$ bangType noUnpackedNoStrict (conT ''Int)
		] ++
		(mkStringField . (: "") <$> ['a' .. 'z']) ++
		(mkStringField . ('l' :) . (: "") <$> ['a' .. 'z']) ++
		[
		varBangType (mkName "tags")
			$ bangType noUnpackedNoStrict (conT ''String),
		varBangType (mkName "content")
			$ bangType noUnpackedNoStrict (conT ''String),
		varBangType (mkName "sig")
			$ bangType noUnpackedNoStrict (conT ''String)
		])
	] [derivClause Nothing [conT ''Show]]

baz d e = recConE d $ [
	('idnt ,) <$> varE 'bsToHexStr `appE` (varE 'Signed.id `appE` varE e),
	('pubkey ,) <$> varE 'bsToHexStr `appE`
		(varE 'BS.tail `appE`
		(varE 'serialize_point `appE`
		(varE 'Signed.pubkey `appE` varE e))),
	('created_at ,) <$> varE 'unixTimeToInt `appE`
		(varE 'Signed.created_at `appE` varE e),
	('kind ,) <$> varE 'Signed.kind `appE` varE e
	] ++ bar e ++ barbar e ++ [
	('tags ,) <$> varE 'etgs `appE` varE e,
	('content ,) <$> varE 'T.unpack `appE`
		(varE 'Signed.content `appE` varE e),
	('sig ,) <$> varE 'bsToHexStr `appE`
		(varE 'Signed.sig `appE` varE e)
	]

unixTimeToInt :: UnixTime -> Int
unixTimeToInt ut = let CTime i = toEpochTime ut in fromIntegral i

eventToTag :: Signed.E -> String -> Maybe String
eventToTag e = (T.unpack . fst <$>) . (`Map.lookup` Signed.tags e) . T.pack

bar e = foo e <$> ((: "") <$> ['a' .. 'z'])
barbar e = foo e <$> (('l' :) . (: "") <$> ['a' .. 'z'])

foo e k = (mkName k ,) <$> varE 'eventToTag `appE` varE e `appE` litE (StringL k)

etgs :: Signed.E -> String
etgs = LBSC.unpack . A.encode . EvJsn.encodeTags . Signed.tags

-- TRY IT
-- ghci> ev <- getSampleSigned "/path/to/sec_file" "/path/to/pub_file"
-- ghci> :set -XTemplateHaskell
-- ghci> $(baz (mkName "Foo") 'ev)
