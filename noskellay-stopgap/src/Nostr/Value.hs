{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Nostr.Value where

import Data.Text qualified as T
import Database.SmplstSQLite3

data Value = VText T.Text deriving Show

instance Bindable Value where bindN' sm n (VText t) = bindN' sm n t
