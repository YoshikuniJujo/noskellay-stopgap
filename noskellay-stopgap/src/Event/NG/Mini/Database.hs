{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Mini.Database where

import "try-nostr-event-ng" Nostr.Event.Signed qualified as Signed

import Event.NG.Mini.Database.Type

fromSigned :: Signed.E -> IO (E, [Tag])
fromSigned e = undefined
