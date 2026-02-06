module Main where

import Database.SmplstSQLite3

main :: IO ()
main = do
	putStrLn "CREATE TABLE"
	withSQLite "foo_mini.sqlite3" $ \db -> do
		withPrepared db (
			"CREATE TABLE events(" ++
			"id PRIMARY KEY, pubkey, created_at, kind, " ++
			(('"' :) . (: "\", ") =<< ['a' .. 'c']) ++
			"tags, content, sig, verified)" ) step
	pure ()
