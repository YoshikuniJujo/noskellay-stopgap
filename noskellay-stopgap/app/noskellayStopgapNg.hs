{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Arrow
import Control.Concurrent
import Data.Function
import Data.Foldable
import Data.Traversable
import Data.Maybe
import Data.Text qualified as T
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Network.WebSockets
import Database.SmplstSQLite3 qualified as SQL

import "try-nostr-event-ng" Nostr.Event.Json qualified as EvJsn
import Event.NG.Database qualified as Db

import Nostr.Database.Filter
import Nostr.Filter.Json qualified as FltJsn

main :: IO ()
main = SQL.withSQLite "foo_ng.sqlite3" realMain

realMain :: SQL.SQLite -> IO ()
realMain db = runServer "0.0.0.0" 10000 \pconn -> acceptRequest pconn >>= \conn -> do
	fix \go -> receive conn >>= \case
		r@(DataMessage _ _ _ (Text rjsn _)) -> do
			print r
			s <- maybe (pure Nothing) (recToSend db) $ decode rjsn
			(>> go) case s of
				Nothing -> pure ()
				Just ((encode <$>) -> sjsns) -> do
					putStrLn "FOOO"
					print sjsns
					putStrLn ""
--					(\snd -> sendDataMessage conn $ Text snd Nothing) `mapM_` sjsns
--					sendDataMessages conn . tail $ (\snd -> Text snd Nothing) <$> sjsns
					sendDataMessages conn $ (\snd -> Text snd Nothing) <$> sjsns
		r@(ControlMessage (Close _ _)) -> print r
		r -> print r >> go
	sendClose conn ("Good-bye!" :: T.Text)

recToSend :: SQL.SQLite -> Value -> IO (Maybe [Value])
recToSend db = \case
	Array (toList -> String "EVENT" :
			Object ((id &&& KM.lookup "id") -> (ev, Just (String i))) : _) -> do
		Just (ev', ts) <- maybe (pure Nothing) ((Just <$>) . Db.fromSigned) $ EvJsn.decode' ev
		_ <- Db.insert db ev'
		uncurry (Db.insertTags db) `mapM_` ts
		print ev'
		pure $ Just [
			Array [String "OK", String i, Bool True, String ""]
			]
	Array (toList -> String "REQ" : String i : fs) -> do
		forkIO $ putStrLn "OOPS!"
		putStrLn $ "FILTERS: " ++ show fs
		putStrLn $ "FILTERS: " ++ show (filtersToFilter $ mapMaybe FltJsn.decode fs)
--		(evs, _) <- Db.selectAll db
		putStrLn $ "OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOPS!!!"
		print . FltJsn.decode $ head fs
		putStrLn $ "PPPPPPPPPPOOOOOOOOOOOOOOOOOOOOOOOOOOOOPS!!!"
		print . (filterToFilter <$>) . FltJsn.decode $ head fs
--		(evs, _) <- Db.selectAllFilter db . fromJust . FltJsn.decode $ head fs
		(evs, _) <- Db.selectAllFilters db $ mapMaybe FltJsn.decode fs
		print evs
		let	ev' = (<$> evs) \ev -> maybe Nothing (\e ->
				(Just $ Array [String "EVENT", String i, Object e]))
				(EvJsn.encode' ev)
		print ev'
		pure . Just $ catMaybes ev' ++
			[Array [String "EOSE", String i]]
	_ -> pure Nothing
