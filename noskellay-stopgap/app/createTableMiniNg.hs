{-# LANGUAGE MultilineStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Database.SmplstSQLite3
import Event.NG.Mini.Database.Abc

main :: IO ()
main = do
	putStrLn "CREATE TABLE"
	(print =<<) . withSQLite ("foo_mini_ng_" ++ abc ++ ".sqlite3") $ \db -> do
		_ <- withPrepared db (
			"CREATE TABLE events(" ++
			"uuid_v7_high, uuid_v7_low, " ++
			"id, pubkey, created_at, kind, " ++
			concat (concat $ zipWith (\a b -> [a, b]) foo bar) ++
			"tags, content, sig, verified, " ++
			"PRIMARY KEY(uuid_v7_high, uuid_v7_low))"
			) step
		(\c -> withPrepared db (createTableTags c) step) `mapM_` abc

createTableTags :: Char -> String
createTableTags c = "CREATE TABLE tags_" ++ [c] ++ "(uuid_h, uuid_l, value)"

foo, bar :: [String]
foo = ('"' :) . (: "_h\", ") <$> abc
bar = ('"' :) . (: "_l\", ") <$> abc
