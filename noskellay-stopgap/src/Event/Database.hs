{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Database where

import Prelude hiding (id)
import Language.Haskell.TH
import Nostr.Event.Signed qualified as Signed

import Event.Database.TH
import Event.Database.Type

do	e' <- newName "e"
	sequence [
		sigD (mkName "fromSigned")
			$ arrowT `appT` conT ''Signed.E `appT` conT ''E,
		funD (mkName "fromSigned")
			[clause [varP e'] (normalB $ baz 'E e') []]
		]

-- TRY IT
-- ghci> ev <- getSampleSigned "/path/to/sec_file" "/path/to/pub_file"
-- ghci> fromSigned ev
