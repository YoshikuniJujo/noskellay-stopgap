{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Nostr.Event qualified as Event
import Nostr.Event.Signed qualified as Signed

getSampleSigned :: FilePath -> FilePath -> IO Signed.E
getSampleSigned scfp pbfp = do
	Just sc <- Event.secretFromBech32 . T.init <$> T.readFile scfp
	Signed.signature sc =<< Event.sample pbfp
