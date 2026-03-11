{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.ByteString.Lazy.Char8 qualified as BSLC
import Data.Aeson qualified as A
import System.Environment
import Database.SmplstSQLite3
import Event.NG.Mini.Database
import SamplesNG

import "try-nostr-event-ng" Nostr.Event.Json qualified as EvJsn

import Event.NG.Mini.Database.Abc

main :: IO ()
main = do
	secfp : pubfp : _ <- getArgs
	putStrLn "INSERT TO TABLE"
	ev <- getSample secfp pubfp
	Just evjsn <- pure $ A.encode <$> EvJsn.encode' ev
	Just evs <- pure $ EvJsn.decode' =<< A.decode evjsn
	print $ ev == evs
	BSLC.putStrLn evjsn
	putStrLn ""
	ev' <- fromSigned evs
	withSQLite ("foo_mini_ng_" ++ abc ++ ".sqlite3") $ \db -> do
		_ <- insert db (fst ev')
		_ <- uncurry (insertTags db) `mapM` snd ev'
		(r, _) <- selectAll db
		maybe (pure ()) BSLC.putStrLn $ A.encode <$> EvJsn.encode' (head r)
		print $ length r
		print =<< count db
