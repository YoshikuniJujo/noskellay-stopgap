{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Mini.Database.TH where

import Language.Haskell.TH

import Event.Mini.Database.Type qualified as T

import Control.Monad
import Data.Function
import Database.SmplstSQLite3

columns :: [Name]
columns = beforeAToZ ++ (mkName . (: "") <$> aToZ) ++ afterAToZ

beforeAToZ, afterAToZ :: [Name]
beforeAToZ = ['T.idnt, 'T.pubkey, 'T.created_at, 'T.kind]
afterAToZ = ['T.tags, 'T.content, 'T.sig, 'T.verified]

aToZ :: String
aToZ = ['a' .. 'c']
