{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Mini.Database.TH where

import Language.Haskell.TH
import Database.SmplstSQLite3 hiding (Stmt)

import Nostr.Event.Signed
import Event.Mini.Database.Type qualified as T

binds :: Quote m => Name -> Name -> [(Integer, Name)] -> [m Stmt]
binds sm e = (uncurry (bind1 sm e) <$>)

bind1 :: Quote m => Name -> Name -> Integer -> Name -> m Stmt
bind1 sm e n fld = noBindS $ varE 'bindN `appE` varE sm
	`appE` (litE $ IntegerL n) `appE` (varE fld `appE` varE e)

columns :: [Name]
columns = beforeAToZ ++ (mkName . (: "") <$> aToZ) ++ afterAToZ

beforeAToZ, afterAToZ :: [Name]
beforeAToZ = ['T.idnt, 'T.pubkey, 'T.created_at, 'T.kind]
afterAToZ = ['T.tags, 'T.content, 'T.sig, 'T.verified]

aToZ :: String
aToZ = ['a' .. 'c']

mkSelectAll :: Quote m => Name -> Name -> Name -> m Exp
mkSelectAll sm le ts = do
	vs <- newName `mapM` replicate (length columns) "x"
	r <- newName "r"
	e <- newName "e"
	doE $ (bindS (varP r) $ varE 'step `appE` varE sm) :
		zipWith (foo sm) vs [0 ..] ++ [
		letS [valD (varP e) (normalB (
			recConE le $ zipWith (\f v -> (f ,) <$> varE v) columns vs
			)) []],
		noBindS $ varE 'pure `appE` (
			tupE [	varE r,
				varE ts `appE` varE e] )
		]

foo sm v n = bindS (varP v) $ varE 'column `appE` varE sm `appE` litE (integerL n)
