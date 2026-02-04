{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Database where

import Control.Arrow
import Data.Map qualified as Map
import Data.Text qualified as T
import Language.Haskell.TH
import ToolsTH
import Nostr.Event.Signed qualified as Signed

(: []) <$> dataD (cxt []) (mkName "Foo") [] Nothing [
	recC (mkName "Foo") ([
		varBangType (mkName "id")
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
		varBangType (mkName "content")
			$ bangType noUnpackedNoStrict (conT ''String),
		varBangType (mkName "sig")
			$ bangType noUnpackedNoStrict (conT ''String),
		varBangType (mkName "tags")
			$ bangType noUnpackedNoStrict (conT ''String)
		])
	] [derivClause Nothing [conT ''Show]]

baz d e = recConE d [
	(mkName "id" ,) <$> varE 'Map.lookup `appE` varE (mkName "id") `appE` varE e
	]

eventToTag :: Signed.E -> String -> Maybe String
eventToTag e = (T.unpack . fst <$>) . (`Map.lookup` Signed.tags e) . T.pack

bar e = foo e <$> ((: "") <$> ['a' .. 'z'])

foo e k = (mkName k ,) <$> varE 'eventToTag `appE` varE e `appE` litE (StringL k)
