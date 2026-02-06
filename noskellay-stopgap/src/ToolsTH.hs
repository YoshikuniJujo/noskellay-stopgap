{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ToolsTH (noUnpackedNoStrict, mkStringField) where

import Language.Haskell.TH

noUnpackedNoStrict :: Quote m => m Bang
noUnpackedNoStrict = bang noSourceUnpackedness noSourceStrictness

mkStringField :: Quote m => String -> m VarBangType
mkStringField nm = varBangType (mkName nm)
	. bangType noUnpackedNoStrict $ (conT ''Maybe) `appT` (conT ''String)
