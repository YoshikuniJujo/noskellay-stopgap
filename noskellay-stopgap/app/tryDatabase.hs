{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Database.SmplstSQLite3
import System.Environment

import Event.Database
import Samples

main :: IO ()
main = do
	secfp : pubfp : _ <- getArgs
	putStrLn "INSERT TO TABLE"
	ev <- getSampleSigned secfp pubfp
	print ev
	let	ev' = fromSigned ev
	print ev'
	withSQLite "foo.sqlite3" $ \db -> do
		print =<< insert db ev'
		putStrLn ""
		print =<< selectAll1 db
