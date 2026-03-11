{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Mini.Database where

import Control.Monad
import Data.Maybe
import Data.Map qualified as Map
import Data.Char
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text qualified as T
import Data.Aeson qualified as A
import Data.UUIDv7
import Crypto.Curve.Secp256k1
import Database.SmplstSQLite3
import "try-nostr-event-ng" Nostr.Event.Signed qualified as Signed
import "try-nostr-event-ng" Nostr.Event.Json qualified as EvJsn
import Event.Database.TH
import Event.NG.Mini.Database.Type
import Event.NG.Mini.Database.TH qualified as Mini
import Event.NG.Database.Common.TH qualified as Common

import Event.Database.Tools

import Event.NG.Database.Common
import Event.NG.Database.Common.Tag

import Event.NG.Mini.Database.Abc

fromSigned :: Signed.E -> IO (E, [(T.Text, Tag)])
fromSigned e = do
	dct <- genId $ fromAbc abc
	e' <- fromSignedE dct e
	pure (e', tagsToTags dct $ Signed.tags e)

fromAbc :: String -> [T.Text]
fromAbc [] = []
fromAbc (c : cs)
	| isUpper c = ("u" T.:> toLower c) : fromAbc cs
	| otherwise = ("" T.:> c) : fromAbc cs

genId :: [T.Text] -> IO (Map.Map T.Text UUIDv7)
genId a = Map.fromList <$> zipWith (,) a <$> replicateM 3 nextUUIDv7

fromSignedE :: Map.Map T.Text UUIDv7 -> Signed.E -> IO E
fromSignedE dct e = $(Common.mkFromSignedEEx' Mini.mkE ["a", "b", "c"] 'dct 'e)

toSigned :: E -> Signed.E
toSigned e = Signed.E {
	Signed.idnt = idnt e,
	Signed.pubkey = fromJust . parse_point $ pubkey e,
	Signed.created_at = intToUnixTime $ created_at e,
	Signed.kind = kind e,
	Signed.tags = EvJsn.decodeTags . fromJust . A.decode . LBSC.pack $ tags e,
	Signed.content = T.pack $ content e,
	Signed.sig = sig e,
	Signed.verified = verified e }

insert :: SQLite -> E -> IO (Result, String)
insert db ev = withPrepared db (insertCommand Mini.columns) \sm ->
	$(mkInsert Mini.columns 'sm 'ev)

insertTags :: SQLite -> T.Text -> Tag -> IO (Result, String)
insertTags db nm tg =
	withPrepared db (insertCommand' ("tags_" ++ T.unpack nm) columnsTag) \sm ->
		$(mkInsert columnsTag 'sm 'tg)

selectAll :: SQLite -> IO ([Signed.E], String)
selectAll db = withPrepared db "SELECT * FROM events" \sm ->
	$(mkSelectAll Mini.columns 'sm 'E 'toSigned)

count :: SQLite -> IO (Int, String)
count db = withPrepared db "SELECT COUNT(*) FROM events" \sm -> do
	_ <- step sm
	column sm 0
