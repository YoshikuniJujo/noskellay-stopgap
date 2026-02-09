{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Database.SmplstSQLite3
import System.Environment

import Event.Mini.Database
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
		print =<< insert db ev'
		putStrLn ""
		(r, _) <- selectAll db
		print r
		print $ length r
		print =<< count db
