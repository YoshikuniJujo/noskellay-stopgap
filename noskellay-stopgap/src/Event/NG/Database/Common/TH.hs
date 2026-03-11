{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Database.Common.TH (mkFromSignedEEx') where

import Language.Haskell.TH
import Data.Map qualified as Map

import Data.UUIDv7

mkFromSignedEEx' ::
	(Name -> Name -> Name -> [String] -> [Name] -> ExpQ) ->
	[String] -> Name -> Name -> ExpQ
mkFromSignedEEx' mk abc dct e = do
	(uh, ul) <- (,) <$> newName "uh" <*> newName "ul"
	us <- (newName . ('u' :)) `mapM` abc
	doE [	bindS	(tupP [varP uh, varP ul])
			(uInfixE (varE 'toInts) (varE '(<$>)) (varE 'nextUUIDv7)),
		letS $ zipWith (mkFromUUIDv7 dct) us abc,
		noBindS $ varE 'pure `appE` mk uh ul e abc us ]

mkFromUUIDv7 :: Name -> Name -> String -> DecQ
mkFromUUIDv7 dct nm c = valD (varP nm) (normalB
	$ varE 'toInts `appE`
		uInfixE (varE dct) (varE '(Map.!)) (litE $ stringL c)) []
