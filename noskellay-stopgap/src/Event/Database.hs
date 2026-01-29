{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Database where

import Language.Haskell.TH
import ToolsTH

(: []) <$> dataD (cxt []) (mkName "Foo") [] Nothing [
	recC (mkName "Foo") ([
		varBangType (mkName "id")
			$ bangType noUnpackedNoStrict (conT ''String),
		varBangType (mkName "pubkey")
			$ bangType noUnpackedNoStrict (conT ''String),
		varBangType (mkName "created_at")
			$ bangType noUnpackedNoStrict (conT ''Int) ] ++ [
		varBangType (mkName "kind")
			$ bangType noUnpackedNoStrict (conT ''Int),
		varBangType (mkName "a")
			$ bangType noUnpackedNoStrict (conT ''String),
		varBangType (mkName "b")
			$ bangType noUnpackedNoStrict (conT ''String),
		mkStringField "c"
		] ++ [
		varBangType (mkName "content")
			$ bangType noUnpackedNoStrict (conT ''String),
		varBangType (mkName "sig")
			$ bangType noUnpackedNoStrict (conT ''String),
		varBangType (mkName "tags")
			$ bangType noUnpackedNoStrict (conT ''String)
		])
	] [derivClause Nothing [conT ''Show]]
