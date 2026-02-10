{-# LANGUAGE MultilineStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Database.SmplstSQLite3

main :: IO ()
main = do
	putStrLn "CREATE TABLE"
	(print =<<) . withSQLite "foo_mini_new.sqlite3" $ \db -> do
		withPrepared db (
			"CREATE TABLE events(" ++
			"id PRIMARY KEY, pubkey, created_at, kind, " ++
			(('"' :) . (: "\", ") =<< ['a' .. 'c']) ++
			"tags, content, sig, verified)" ) step
		withPrepared db """
			CREATE TABLE tags(uuid, value)
			""" step
