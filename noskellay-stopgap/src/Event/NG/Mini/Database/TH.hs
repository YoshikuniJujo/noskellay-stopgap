{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Mini.Database.TH where

import Language.Haskell.TH
import Event.NG.Mini.Database.Type qualified as T
import Event.NG.Database.Common qualified as TC

columns :: [Name]
columns = beforeAToZ ++
	(mkName <$> (hl =<< (: "") <$> "abc")) ++ afterAToZ

hl :: String -> [String]
hl c = [c ++ "h", c ++ "l"]

columnsTag :: [Name]
columnsTag = ['TC.uuidH, 'TC.uuidL, 'TC.tagValue]

beforeAToZ, afterAToZ :: [Name]
beforeAToZ = [
	'T.uuidV7High, 'T.uuidV7Low,
	'T.idnt, 'T.pubkey, 'T.created_at, 'T.kind ]
afterAToZ = ['T.tags, 'T.content, 'T.sig, 'T.verified]
