{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Function
import Data.Foldable
import Data.Text qualified as T
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Network.WebSockets

main :: IO ()
main = runServer "0.0.0.0" 10000 \pconn -> acceptRequest pconn >>= \conn -> do
	fix \go -> receive conn >>= \case
		r@(DataMessage _ _ _ (Text rjsn _)) -> do
			print r
			(>> go) case recToSend =<< decode rjsn of
				Nothing -> pure ()
				Just (encode -> sjsn) ->
					sendDataMessage conn $ Text sjsn Nothing
		r@(ControlMessage (Close _ _)) -> print r
		r -> print r >> go
	sendClose conn ("Good-bye!" :: T.Text)

recToSend :: Value -> Maybe Value
recToSend = \case
	Array (toList ->
		String "EVENT" :
			Object (KM.lookup "id" -> Just (String i)) : _) ->
		Just $ Array [String "OK", String i, Bool True, String ""]
	Array (toList -> String "REQ" : String i : _) ->
		Just $ Array [String "EOSE", String i]
	_ -> Nothing
