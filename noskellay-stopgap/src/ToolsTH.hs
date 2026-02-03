{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ToolsTH where

import Language.Haskell.TH

noUnpackedNoStrict :: Quote m => m Bang
noUnpackedNoStrict = bang noSourceUnpackedness noSourceStrictness

mkStringField nm = varBangType (mkName nm)
	. bangType noUnpackedNoStrict $ (conT ''Maybe) `appT` (conT ''String)
