{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Mini.Database.TH (mkE, columns) where

import Language.Haskell.TH
import Data.Char
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text qualified as T
import Data.Aeson qualified as A
import Crypto.Curve.Secp256k1
import Event.NG.Mini.Database.Type qualified as T

import "try-nostr-event-ng" Nostr.Event.Signed qualified as Signed
import "try-nostr-event-ng" Nostr.Event.Json qualified as EvJsn

import Event.Database.Tools
import Event.NG.Database.Common.HL

import Event.NG.Mini.Database.Abc

columns :: [Name]
columns = beforeAToZ ++
	(mkName <$> (hl =<< fromAbc abc)) ++ afterAToZ

fromAbc :: String -> [String]
fromAbc = \case
	"" -> []
	c : cs	| isUpper c -> ('u' : [toLower c]) : fromAbc cs
		| otherwise -> [c] : fromAbc cs

beforeAToZ, afterAToZ :: [Name]
beforeAToZ = [
	'T.uuidV7High, 'T.uuidV7Low,
	'T.idnt, 'T.pubkey, 'T.created_at, 'T.kind ]
afterAToZ = ['T.tags, 'T.content, 'T.sig, 'T.verified]

mkE :: Name -> Name -> Name -> [String] -> [Name] -> ExpQ
mkE uh ul e a ts = recConE 'T.E $ [
	('T.uuidV7High ,) <$> varE uh,
	('T.uuidV7Low ,) <$> varE ul,
	('T.idnt ,) <$> varE 'Signed.idnt `appE` varE e,
	('T.pubkey ,) <$> varE 'BS.tail `appE`
		(varE 'serialize_point `appE`
			(varE 'Signed.pubkey `appE` varE e)),
	('T.created_at ,) <$> varE 'unixTimeToInt `appE`
		(varE 'Signed.created_at `appE` varE e),
	('T.kind ,) <$> varE 'Signed.kind `appE` varE e
	] ++ mkETags a ts ++ [
	('T.tags ,) <$> varE 'LBSC.unpack `appE`
		(varE 'A.encode `appE`
			(varE 'EvJsn.encodeTags `appE`
				(varE 'Signed.tags `appE` varE e))),
	('T.content ,) <$> varE 'T.unpack `appE`
		(varE 'Signed.content `appE` varE e),
	('T.sig ,) <$> varE 'Signed.sig `appE` varE e,
	('T.verified ,) <$> varE 'Signed.verified `appE` varE e ]
