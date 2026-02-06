{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Database.Tools where

import Foreign.C.Types
import Data.Map qualified as Map
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text qualified as T
import Data.Aeson qualified as A
import Data.UnixTime
import Nostr.Event.Signed qualified as Signed
import Nostr.Event.Json qualified as EvJsn

etgs :: Signed.E -> String
etgs = LBSC.unpack . A.encode . EvJsn.encodeTags . Signed.tags

unixTimeToInt :: UnixTime -> Int
unixTimeToInt ut = let CTime i' = toEpochTime ut in fromIntegral i'

eventToTag :: Signed.E -> String -> Maybe String
eventToTag e' = (T.unpack . fst <$>) . (`Map.lookup` Signed.tags e') . T.pack
