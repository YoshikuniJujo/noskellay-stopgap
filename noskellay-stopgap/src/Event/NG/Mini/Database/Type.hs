{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Mini.Database.Type where

import Event.NG.Mini.Database.Abc
import Event.NG.Database.Common.Type.TH

(: []) <$> mkDataE abc
