{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TrySQL where

import Control.Arrow
import Data.Maybe
import Data.ByteString qualified as BS
import System.Random
import Database.SmplstSQLite3
import "try-nostr-event-ng" Nostr.Event.Signed qualified as Signed
import Event.NG.Mini.Database
import Event.NG.Mini.Database.Abc

import Nostr.Value

import Nostr.Filter qualified as Filter
import Nostr.Database.Filter

allEvents :: IO ([Signed.E], String)
allEvents = withSQLite ("foo_mini_ng_" ++ abc ++ ".sqlite3") $ \db -> do
	selectAll db

allEvents' :: String -> [Value] -> IO ([Signed.E], String)
allEvents' wh vs = withSQLite ("foo_mini_ng_" ++ abc ++ ".sqlite3") $ \db -> do
	selectAll' db wh vs

allEvents'' :: String -> [Value] -> Int -> IO ([Signed.E], String)
allEvents'' wh vs lmt = withSQLite ("foo_mini_ng_" ++ abc ++ ".sqlite3") $ \db -> do
	selectAll'' db wh vs lmt

randomEventId :: IO BS.ByteString
randomEventId = do
	(ids, _) <- first (Signed.idnt <$>) <$> allEvents
	idx <- randomRIO (0, length ids - 1)
	pure $ ids !! idx

-- selectEventWithId :: BS.ByteString -> IO ([Signed.E], String)
selectEventWithId :: BS.ByteString -> IO (BS.ByteString, String)
selectEventWithId idnt = withSQLite ("foo_mini_ng_" ++ abc ++ ".sqlite3") $ \db -> do
	withPrepared db "SELECT * FROM events where id = ?" \sm -> do
		bindN sm 1 idnt
		step sm
		column sm 4

filterEvents :: Filter.Filter -> IO ([Signed.E], String)
filterEvents =
	uncurry allEvents' . sqlWhere selToSql . filterToFilter

filterEvents' :: Filter.Filter -> IO ([Signed.E], String)
filterEvents' f =
	uncurry allEvents'' (sqlWhere selToSql $ filterToFilter f)
		$ fromMaybe 500 (Filter.limit f)

filtersEvents :: [Filter.Filter] -> IO ([Signed.E], String)
filtersEvents fs =
	uncurry allEvents'' (sqlWhere selToSql $ filtersToFilter fs)
		$ maybe 500 sum (sequence $ Filter.limit <$> fs)

showSqlWhere :: Filter.Filter -> (String, [Value])
showSqlWhere = sqlWhere selToSql . filterToFilter
