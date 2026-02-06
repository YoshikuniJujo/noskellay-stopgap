{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.ByteString.Lazy qualified as LBS
import Data.Aeson qualified as A
import Database.SmplstSQLite3
import Nostr.Event.Json qualified as EvJsn
import Event.Mini.Database
import System.Environment

import Samples

main :: IO ()
main = do
	secfp : pubfp : _ <- getArgs
	putStrLn "INSERT TO TABLE"
	ev <- getSampleSigned secfp pubfp
	print ev
	let	ev' = fromSigned ev
	print ev'
	withSQLite "foo_mini.sqlite3" $ \db -> do
		_ <- withPrepared db
			"INSERT INTO events VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" \sm -> do
			bindN sm 1 $ idnt ev'
			bindN sm 2 $ pubkey ev'
			bindN sm 3 $ created_at ev'
			bindN sm 4 $ kind ev'
			bindN sm 5 $ a ev'
			bindN sm 6 $ b ev'
			bindN sm 7 $ c ev'
			bindN sm 8 $ tags ev'
			bindN sm 9 $ content ev'
			bindN sm 10 $ sig ev'
			step sm
		putStrLn ""
		withPrepared db
			"SELECT * FROM events" \sm -> do
			_ <- step sm
			(idnt' :: String) <- column sm 0
			(pk :: String) <- column sm 1
			(crat :: Int) <- column sm 2
			(knd :: Int) <- column sm 3
			(a' :: Maybe String) <- column sm 4
			(b' :: Maybe String) <- column sm 5
			(c' :: Maybe String) <- column sm 6
			tgs <- column sm 7
			{-
			tgs <- (EvJsn.decodeTags <$>)
				. A.decode . LBS.fromStrict <$> column sm 7
				-}
			(cnt :: String) <- column sm 8
			(sg :: String) <- column sm 9
			print $ E {
				idnt = idnt',
				pubkey = pk,
				created_at = crat,
				kind = knd,
				a = a',
				b = b',
				c = c',
				tags = tgs,
				content = cnt,
				sig = sg }
	pure ()
