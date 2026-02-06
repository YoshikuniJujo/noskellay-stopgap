{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Database.SmplstSQLite3
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
		withPrepared db
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
	pure ()
