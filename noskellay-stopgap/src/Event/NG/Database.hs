{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Database where

import Control.Monad
import Data.Map qualified as Map
import Data.Char
import Data.Text qualified as T
import Data.UUIDv7
import Database.SmplstSQLite3
import "try-nostr-event-ng" Nostr.Event.Signed qualified as Signed
import Event.Database.TH
import Event.NG.Database.Type
import Event.NG.Database.TH qualified as TH
import Event.NG.Database.Common.TH qualified as Common

import Event.NG.Database.Common
import Event.NG.Database.Common.Tag
import Event.NG.Database.Common.Abc

import Event.NG.Database.Abc

import Nostr.Value
import Nostr.Database.Filter

import Nostr.Filter qualified as Filter

fromSigned :: Signed.E -> IO (E, [(T.Text, Tag)])
fromSigned e = do
	dct <- genId $ fromAbc abc
	e' <- fromSignedE dct e
	pure (e', tagsToTags dct $ Signed.tags e)

genId :: [T.Text] -> IO (Map.Map T.Text UUIDv7)
genId a = Map.fromList <$> zipWith (,) a <$> replicateM (length abc) nextUUIDv7

fromSignedE :: Map.Map T.Text UUIDv7 -> Signed.E -> IO E
fromSignedE dct e = $(Common.mkFromSignedEEx' TH.mkE (fromAbc' abc) 'dct 'e)

insert :: SQLite -> E -> IO (Result, String)
insert db ev = withPrepared db (insertCommand TH.columns) \sm ->
	$(mkInsert TH.columns 'sm 'ev)

insertTags :: SQLite -> T.Text -> Tag -> IO (Result, String)
insertTags db nm tg =
	withPrepared db (insertCommand' ("tags_" ++ T.unpack nm) columnsTag) \sm ->
		$(mkInsert columnsTag 'sm 'tg)

selectAll :: SQLite -> IO ([Signed.E], String)
selectAll db = withPrepared db "SELECT * FROM events" \sm ->
	$(mkSelectAll TH.columns 'sm 'E 'toSigned)

selectAll' :: SQLite -> String -> IO ([Signed.E], String)
selectAll' db wh = withPrepared db ("SELECT * FROM events" ++ leftJoins abc ++ " where " ++ wh) \sm ->
	$(mkSelectAll TH.columns 'sm 'E 'toSigned)

selectAll'' :: SQLite -> String -> [Value] -> Int -> IO ([Signed.E], String)
selectAll'' db wh vs lmt = do
	putStrLn "selectAll'' begin"
	putStrLn ("SELECT * FROM events" ++
		leftJoins abc ++ " where " ++
		wh ++ " ORDER BY created_at DESC")
	print vs
	putStrLn "before prepared"
	withPrepared db ("SELECT * FROM events" ++
		leftJoins abc ++ " where " ++
		wh ++ " ORDER BY created_at DESC") \sm -> do
		putStrLn "SQL prepared"
		zipWithM (bindN' sm) [1..] vs
		putStrLn ""
		$(mkSelectAll' TH.columns 'lmt 'sm 'E 'toSigned)

selectAllFilter :: SQLite -> Filter.Filter -> IO ([Signed.E], String)
selectAllFilter db f = do
	putStrLn "selectAllFilter begin"
	r <- uncurry (selectAll'' db) (sqlWhere selToSql $ filterToFilter f)
		$ maybe 500 id (Filter.limit f)
	putStrLn "selectAllFilter end"
	pure r

selectAllFilters :: SQLite -> [Filter.Filter] -> IO ([Signed.E], String)
selectAllFilters db fs =
	uncurry (selectAll'' db) (sqlWhere selToSql $ filtersToFilter fs)
		$ maybe 500 sum (sequence $ Filter.limit <$> fs)

count :: SQLite -> IO (Int, String)
count db = withPrepared db "SELECT COUNT(*) FROM events" \sm -> do
	_ <- step sm
	column sm 0
