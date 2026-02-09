{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Database (

	fromSigned,
	insert,
	selectAll1

	) where

import Prelude hiding (id)
import Language.Haskell.TH
import Data.Maybe
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text qualified as T
import Data.Aeson qualified as A
import Database.SmplstSQLite3
import Crypto.Curve.Secp256k1
import Nostr.Event.Signed qualified as Signed
import Nostr.Event.Json qualified as EvJsn

import Event.Database.TH
import Event.Database.Type
import Event.Database.Tools
import Tools

do	e' <- newName "e"
	sequence [
		sigD (mkName "fromSigned")
			$ arrowT `appT` conT ''Signed.E `appT` conT ''E,
		funD (mkName "fromSigned")
			[clause [varP e'] (normalB $ baz 'E e') []]
		]

-- TRY IT
-- ghci> ev <- getSampleSigned "/path/to/sec_file" "/path/to/pub_file"
-- ghci> fromSigned ev

toSigned :: E -> Signed.E
toSigned e = Signed.E {
	Signed.id = hexStrToBs $ idnt e,
	Signed.pubkey = fromJust . parse_point . hexStrToBs $ pubkey e,
	Signed.created_at = intToUnixTime $ created_at e,
	Signed.kind = kind e,
	Signed.tags = EvJsn.decodeTags . fromJust . A.decode . LBSC.pack $ tags e,
	Signed.content = T.pack $ content e,
	Signed.sig = hexStrToBs $ sig e,
	Signed.verified = verified e }

insert db ev = withPrepared db (insertCommand columns) \sm ->
	$(mkInsert columns 'sm 'ev)

selectAll1 db = withPrepared db "SELECT * FROM events" \sm ->
	$(mkSelectAll columns 'sm 'E 'toSigned)
