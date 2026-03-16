{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Database.Common.Type.TH where

import Data.Maybe
import Data.Int
import Data.Char
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text qualified as T
import Data.Aeson qualified as A
import Language.Haskell.TH
import Crypto.Curve.Secp256k1
import "try-nostr-event-ng" Nostr.Event.Signed qualified as Signed
import "try-nostr-event-ng" Nostr.Event.Json qualified as EvJsn
import Event.Database.Tools

mkDataE :: String -> DecQ
mkDataE abc = dataD (pure []) (mkName "E") [] Nothing [
	recC (mkName "E") $ [
		varType "uuidV7High" ''Int64,
		varType "uuidV7Low" ''Int64,
		varType "idnt" ''BS.ByteString,
		varType "pubkey" ''BS.ByteString,
		varType "created_at" ''Int,
		varType "kind" ''Int
		] ++ ((`varType` ''Int64) <$> concat (hl . checkUpper <$> abc)) ++ [
		varType "tags" ''String,
		varType "content" ''String,
		varType "sig" ''BS.ByteString,
		varType "verified" ''Bool
		]
	] [derivClause Nothing [conT ''Show]]

hl :: String -> [String]
hl c = [c ++ "h", c ++ "l"]

checkUpper :: Char -> String
checkUpper = \case c | isUpper c -> 'u' : [toLower c] | otherwise -> [c]

varType :: String -> Name -> VarBangTypeQ
varType nm tp = varBangType (mkName nm) $ bangType noBang (conT tp)

noBang :: BangQ
noBang = bang noSourceUnpackedness noSourceStrictness

mkToSigned :: DecsQ
mkToSigned = sequence [
	sigD (mkName "toSigned")
		$ arrowT `appT` conT (mkName "E") `appT` conT ''Signed.E,
	funD (mkName "toSigned") [do
		e <- newName "e"
		clause [varP e] (normalB $ mkToSignedBody e) []]
	]

mkToSignedBody :: Name -> ExpQ
mkToSignedBody e = recConE 'Signed.E [
	('Signed.idnt ,) <$> varE (mkName "idnt") `appE` varE e,
	('Signed.pubkey ,) <$> varE 'fromJust `appE` (
		varE 'parse_point `appE` (
			varE (mkName "pubkey") `appE` varE e ) ),
	('Signed.created_at ,) <$> varE 'intToUnixTime `appE` (
		varE (mkName "created_at") `appE` varE e ),
	('Signed.kind ,) <$> varE (mkName "kind") `appE` varE e,
	('Signed.tags ,) <$> varE 'EvJsn.decodeTags `appE` (
		varE 'fromJust `appE` (varE 'A.decode `appE` (
			varE 'LBSC.pack `appE` (
				varE (mkName "tags") `appE` varE e ) )) ),
	('Signed.content ,) <$> varE 'T.pack `appE` (
		varE (mkName "content") `appE` varE e ),
	('Signed.sig ,) <$> varE (mkName "sig") `appE` varE e,
	('Signed.verified ,) <$> varE (mkName "verified") `appE` varE e ]
