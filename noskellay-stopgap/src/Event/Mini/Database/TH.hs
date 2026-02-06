{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Mini.Database.TH where

import Language.Haskell.TH
import Database.SmplstSQLite3 hiding (Stmt)

import Event.Mini.Database.Type

binds :: Quote m => Name -> Name -> [(Integer, Name)] -> [m Stmt]
binds sm e = (uncurry (bind1 sm e) <$>)

bind1 :: Quote m => Name -> Name -> Integer -> Name -> m Stmt
bind1 sm e n fld = noBindS $ varE 'bindN `appE` varE sm
	`appE` (litE $ IntegerL n) `appE` (varE fld `appE` varE e)

columns :: [Name]
columns = beforeAToZ ++ (mkName . (: "") <$> aToZ) ++ afterAToZ

beforeAToZ, afterAToZ :: [Name]
beforeAToZ = ['idnt, 'pubkey, 'created_at, 'kind]
afterAToZ = ['tags, 'content, 'sig, 'verified]

aToZ :: String
aToZ = ['a' .. 'c']
