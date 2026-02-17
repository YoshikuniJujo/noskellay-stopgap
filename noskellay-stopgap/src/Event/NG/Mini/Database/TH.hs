{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Event.NG.Mini.Database.TH where

import Language.Haskell.TH
import Event.NG.Mini.Database.Type qualified as T

columns :: [Name]
columns = beforeAToZ ++
	(mkName <$> ["ah", "al", "bh", "bl", "ch", "cl"]) ++ afterAToZ

columnsTag :: [Name]
columnsTag = ['T.uuidH, 'T.uuidL, 'T.tagValue]

beforeAToZ, afterAToZ :: [Name]
beforeAToZ = ['T.idnt, 'T.pubkey, 'T.created_at, 'T.kind]
afterAToZ = ['T.tags, 'T.content, 'T.sig, 'T.verified]
