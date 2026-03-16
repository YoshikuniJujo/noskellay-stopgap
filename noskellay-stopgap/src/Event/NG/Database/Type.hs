{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Database.Type where

import Event.NG.Database.Abc
import Event.NG.Database.Common.Type.TH

(: []) <$> mkDataE abc

mkToSigned
