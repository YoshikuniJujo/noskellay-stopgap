{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Database.Common.Tag (columnsTag) where

import Language.Haskell.TH
import Event.NG.Database.Common qualified as TC

columnsTag :: [Name]
columnsTag = ['TC.uuidH, 'TC.uuidL, 'TC.tagValue]
