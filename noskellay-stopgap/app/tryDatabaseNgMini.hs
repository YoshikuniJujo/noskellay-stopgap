{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import System.Environment
import Database.SmplstSQLite3
import Event.NG.Mini.Database
import SamplesNG

main :: IO ()
main = do
	secfp : pubfp : _ <- getArgs
	putStrLn "INSERT TO TABLE"
	ev <- getSample secfp pubfp
	print ev
	ev' <- fromSigned ev
	print ev'
	withSQLite "foo_mini_ng.sqlite3" $ \db -> do
		print =<< insert db (fst ev')
		print =<< insertTags db `mapM` snd ev'
