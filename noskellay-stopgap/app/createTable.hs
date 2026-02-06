{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Database.SmplstSQLite3

main :: IO ()
main = do
	putStrLn "CREATE TABLE"
	_ <- withSQLite "foo.sqlite3" $ \db -> do
		withPrepared db (
			"CREATE TABLE events(" ++
			"id, pubkey, created_at, kind, " ++
			(('"' :) . (: "\", ") =<< ['a' .. 'z']) ++
			(("\"L" ++) . (: "\", ") =<< ['A' .. 'Z']) ++
			"tags, content, sig)" ) step
	pure ()
