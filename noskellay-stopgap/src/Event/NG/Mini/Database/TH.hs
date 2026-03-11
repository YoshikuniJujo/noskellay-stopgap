{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Mini.Database.TH where

import Language.Haskell.TH
import Data.Map qualified as Map
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text qualified as T
import Data.Aeson qualified as A
import Crypto.Curve.Secp256k1
import Event.NG.Mini.Database.Type qualified as T
import Event.NG.Database.Common qualified as TC

import "try-nostr-event-ng" Nostr.Event.Signed qualified as Signed
import "try-nostr-event-ng" Nostr.Event.Json qualified as EvJsn

import Event.Database.Tools

import Data.UUIDv7

columns :: [Name]
columns = beforeAToZ ++
	(mkName <$> (hl =<< (: "") <$> "abc")) ++ afterAToZ

hl :: String -> [String]
hl c = [c ++ "h", c ++ "l"]

columnsTag :: [Name]
columnsTag = ['TC.uuidH, 'TC.uuidL, 'TC.tagValue]

beforeAToZ, afterAToZ :: [Name]
beforeAToZ = [
	'T.uuidV7High, 'T.uuidV7Low,
	'T.idnt, 'T.pubkey, 'T.created_at, 'T.kind ]
afterAToZ = ['T.tags, 'T.content, 'T.sig, 'T.verified]

mkE :: Name -> Name -> Name -> [String] -> [Name] -> ExpQ
mkE uh ul e abc ts = recConE 'T.E $ [
	('T.uuidV7High ,) <$> varE uh,
	('T.uuidV7Low ,) <$> varE ul,
	('T.idnt ,) <$> varE 'Signed.idnt `appE` varE e,
	('T.pubkey ,) <$> varE 'BS.tail `appE`
		(varE 'serialize_point `appE`
			(varE 'Signed.pubkey `appE` varE e)),
	('T.created_at ,) <$> varE 'unixTimeToInt `appE`
		(varE 'Signed.created_at `appE` varE e),
	('T.kind ,) <$> varE 'Signed.kind `appE` varE e
	] ++ mkETags abc ts ++ [
	('T.tags ,) <$> varE 'LBSC.unpack `appE`
		(varE 'A.encode `appE`
			(varE 'EvJsn.encodeTags `appE`
				(varE 'Signed.tags `appE` varE e))),
	('T.content ,) <$> varE 'T.unpack `appE`
		(varE 'Signed.content `appE` varE e),
	('T.sig ,) <$> varE 'Signed.sig `appE` varE e,
	('T.verified ,) <$> varE 'Signed.verified `appE` varE e ]

mkETags :: [String] -> [Name] -> [Q (Name, Exp)]
mkETags = (concat .) . zipWith (uncurry mkETag1) . (mkTagName <$>)
	

mkTagName :: String -> (Name, Name)
mkTagName c = (mkName $ c ++ "h", mkName $ c ++ "l")

mkETag1 :: Name -> Name -> Name -> [Q (Name, Exp)]
mkETag1 ah al ua = [
	(ah ,) <$> varE 'fst `appE` varE ua,
	(al ,) <$> varE 'snd `appE` varE ua ]

mkFromSignedEEx :: [String] -> Name -> Name -> ExpQ
mkFromSignedEEx abc dct e = do
	uh <- newName "uh"
	ul <- newName "ul"
	us <- (newName . ('u' :)) `mapM` abc
	doE [
		bindS	(tupP [varP uh, varP ul])
			(uInfixE (varE 'toInts) (varE '(<$>)) (varE 'nextUUIDv7)),
		letS $ zipWith (mkFromUUIDv7 dct) us abc,
		noBindS $ varE 'pure `appE` mkE uh ul e abc us
		]

mkFromUUIDv7 :: Name -> Name -> String -> DecQ
mkFromUUIDv7 dct nm c = valD (varP nm) (normalB
	$ varE 'toInts `appE`
		uInfixE (varE dct) (varE '(Map.!)) (litE $ stringL c)) []
