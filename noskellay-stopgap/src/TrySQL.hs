{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TrySQL where

import Control.Arrow
import Data.ByteString qualified as BS
import System.Random
import Database.SmplstSQLite3
import "try-nostr-event-ng" Nostr.Event.Signed qualified as Signed
import Event.NG.Mini.Database
import Event.NG.Mini.Database.Abc

allEvents :: IO ([Signed.E], String)
allEvents = withSQLite ("foo_mini_ng_" ++ abc ++ ".sqlite3") $ \db -> do
	selectAll db

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
