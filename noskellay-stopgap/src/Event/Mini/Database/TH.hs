{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Mini.Database.TH where

import Language.Haskell.TH

import Event.Mini.Database.Type qualified as T

import Control.Monad
import Data.Function
import Database.SmplstSQLite3

columns :: [Name]
columns = beforeAToZ ++ (mkName . (: "") <$> aToZ) ++ afterAToZ

beforeAToZ, afterAToZ :: [Name]
beforeAToZ = ['T.idnt, 'T.pubkey, 'T.created_at, 'T.kind]
afterAToZ = ['T.tags, 'T.content, 'T.sig, 'T.verified]

aToZ :: String
aToZ = ['a' .. 'c']

mkSelectAll :: Quote m => [Name] -> Name -> Name -> Name -> m Exp
mkSelectAll clmns sm le ts = do
	vs <- replicateM (length clmns) $ newName "x"
	go <- newName "go"
	r <- newName "r"
	e <- newName "e"
	(varE 'fix `appE`) . lamE [varP go] . doE $
		bindS (varP r) (varE 'step `appE` varE sm) : [
		noBindS $ caseE (varE r) [
			match (conP 'Done []) (normalB
				$ varE 'pure `appE` conE '[]) [],
			match (conP 'Row []) (normalB . doE $
				zipWith (foo' sm) vs [0 ..] ++ [
				letS [
					valD (varP e) (normalB (
						recConE le $ zipWith (\f v -> (f ,) <$> varE v) clmns vs
						)) []
					],
				noBindS $ infixE
					(Just $ infixE
						(Just $ varE ts `appE` varE e)
						(conE '(:))
						Nothing)
					(varE '(<$>))
					(Just $ varE go)
				]) [],
			match wildP
				(normalB $ varE 'error `appE` litE (stringL "bad"))
				[]
			]
--		zipWith (foo' sm) vs [0 ..] ++ [
--		noBindS $ varE 'pure `appE` litE (integerL 123)
		]

foo' sm v n = bindS (varP v) $ varE 'column `appE` varE sm `appE` litE (integerL n)
