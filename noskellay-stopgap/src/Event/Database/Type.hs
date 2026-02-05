{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Database.Type (E(..)) where

import Language.Haskell.TH

import ToolsTH

(: []) <$> dataD (cxt []) (mkName "E") [] Nothing [
	recC (mkName "E") ([
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
