{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Database.TH (baz) where

import Foreign.C.Types
import Control.Arrow
import Data.Map qualified as Map
import Data.Char
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text qualified as T
import Data.UnixTime
import Data.Aeson qualified as A
import Language.Haskell.TH
import Nostr.Event.Signed qualified as Signed
import Nostr.Event.Json qualified as EvJsn
import Crypto.Curve.Secp256k1

import Tools
import Event.Database.Type
import Event.Database.Tools

baz :: Quote m => Name -> Name -> m Exp
baz d' e' = recConE d' $ [
	('idnt ,) <$> varE 'bsToHexStr `appE` (varE 'Signed.id `appE` varE e'),
	('pubkey ,) <$> varE 'bsToHexStr `appE`
		(varE 'BS.tail `appE`
		(varE 'serialize_point `appE`
		(varE 'Signed.pubkey `appE` varE e'))),
	('created_at ,) <$> varE 'unixTimeToInt `appE`
		(varE 'Signed.created_at `appE` varE e'),
	('kind ,) <$> varE 'Signed.kind `appE` varE e'
	] ++ bar e' ++ barbar e' ++ [
	('tags ,) <$> varE 'etgs `appE` varE e',
	('content ,) <$> varE 'T.unpack `appE`
		(varE 'Signed.content `appE` varE e'),
	('sig ,) <$> varE 'bsToHexStr `appE`
		(varE 'Signed.sig `appE` varE e')
	]

bar :: Quote m => Name -> [m (Name, Exp)]
bar e' = foo e' <$> ((id &&& id) . (: "") <$> ['a' .. 'z'])

barbar :: Quote m => Name -> [m (Name, Exp)]
barbar e' = foo e' <$> ((('l' :) &&& (toUpper <$>)) . (: "") <$> ['a' .. 'z'])

foo :: Quote m => Name -> (String, String) -> m (Name, Exp)
foo e' (k', k'') = (mkName k' ,)
	<$> varE 'eventToTag `appE` varE e' `appE` litE (StringL k'')
