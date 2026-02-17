{-# LANGUAGE MultilineStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Database.SmplstSQLite3

main :: IO ()
main = do
	putStrLn "CREATE TABLE"
	(print =<<) . withSQLite "foo_mini_ng.sqlite3" $ \db -> do
		withPrepared db (
			"CREATE TABLE events(" ++
			"id PRIMARY KEY, pubkey, created_at, kind, " ++
			concat (concat $ zipWith (\a b -> [a, b]) foo bar) ++
			"tags, content, sig, verified)" ) step
		withPrepared db """
			CREATE TABLE tags_a(uuid_h, uuid_l, value)
			""" step
		withPrepared db """
			CREATE TABLE tags_b(uuid_h, uuid_l, value)
			""" step
		withPrepared db """
			CREATE TABLE tags_c(uuid_h, uuid_l, value)
			""" step

foo = ('"' :) . (: "_h\", ") <$> ['a' .. 'c']
bar = ('"' :) . (: "_l\", ") <$> ['a' .. 'c']
