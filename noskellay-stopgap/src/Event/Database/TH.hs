{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Database.TH (

	baz,
	insertCommand, mkInsert,
	mkSelectAll,

	columns

	) where

import Control.Arrow
import Data.List qualified as L
import Data.Char
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Language.Haskell.TH
import Database.SmplstSQLite3 hiding (Stmt)
import Nostr.Event.Signed qualified as Signed
import Crypto.Curve.Secp256k1

import Tools
import Event.Database.Type qualified as T
import Event.Database.Tools

baz :: Quote m => Name -> Name -> m Exp
baz d' e' = recConE d' $ [
	('T.idnt ,) <$> varE 'bsToHexStr `appE` (varE 'Signed.id `appE` varE e'),
	('T.pubkey ,) <$> varE 'bsToHexStr `appE`
		(varE 'BS.tail `appE`
		(varE 'serialize_point `appE`
		(varE 'Signed.pubkey `appE` varE e'))),
	('T.created_at ,) <$> varE 'unixTimeToInt `appE`
		(varE 'Signed.created_at `appE` varE e'),
	('T.kind ,) <$> varE 'Signed.kind `appE` varE e'
	] ++ bar e' ++ barbar e' ++ [
	('T.tags ,) <$> varE 'etgs `appE` varE e',
	('T.content ,) <$> varE 'T.unpack `appE`
		(varE 'Signed.content `appE` varE e'),
	('T.sig ,) <$> varE 'bsToHexStr `appE`
		(varE 'Signed.sig `appE` varE e'),
	('T.verified ,) <$> varE 'Signed.verified `appE` varE e'
	]

bar :: Quote m => Name -> [m (Name, Exp)]
bar e' = foo e' <$> ((id &&& id) . (: "") <$> ['a' .. 'z'])

barbar :: Quote m => Name -> [m (Name, Exp)]
barbar e' = foo e' <$> ((('l' :) &&& (toUpper <$>)) . (: "") <$> ['a' .. 'z'])

foo :: Quote m => Name -> (String, String) -> m (Name, Exp)
foo e' (k', k'') = (mkName k' ,)
	<$> varE 'eventToTag `appE` varE e' `appE` litE (StringL k'')

insertCommand clmns = "INSERT INTO events VALUES(" ++
	L.intercalate ", "
		((: "") <$> replicate (length clmns) '?') ++
	")"

mkInsert clmns sm ev' = doE $ binds sm ev' (zip [1 ..] clmns) ++
		[noBindS $ varE 'step `appE` varE sm]

binds :: Quote m => Name -> Name -> [(Integer, Name)] -> [m Stmt]
binds sm e = (uncurry (bind1 sm e) <$>)

bind1 :: Quote m => Name -> Name -> Integer -> Name -> m Stmt
bind1 sm e n fld = noBindS $ varE 'bindN `appE` varE sm
	`appE` (litE $ IntegerL n) `appE` (varE fld `appE` varE e)

mkSelectAll :: Quote m => [Name] -> Name -> Name -> Name -> m Exp
mkSelectAll clmns sm le ts = do
	vs <- newName `mapM` replicate (length clmns) "x"
	r <- newName "r"
	e <- newName "e"
	doE $ (bindS (varP r) $ varE 'step `appE` varE sm) :
		zipWith (foo' sm) vs [0 ..] ++ [
		letS [valD (varP e) (normalB (
			recConE le $ zipWith (\f v -> (f ,) <$> varE v) clmns vs
			)) []],
		noBindS $ varE 'pure `appE` (
			tupE [	varE r,
				varE ts `appE` varE e] )
		]

foo' sm v n = bindS (varP v) $ varE 'column `appE` varE sm `appE` litE (integerL n)

columns :: [Name]
columns = beforeAToZ ++
	(mkName . (: "") <$> aToZ) ++
	(mkName . ('l' :) . (: "") <$> aToZ) ++
	afterAToZ

beforeAToZ, afterAToZ :: [Name]
beforeAToZ = ['T.idnt, 'T.pubkey, 'T.created_at, 'T.kind]
afterAToZ = ['T.tags, 'T.content, 'T.sig, 'T.verified]

aToZ :: String
aToZ = ['a' .. 'z']
