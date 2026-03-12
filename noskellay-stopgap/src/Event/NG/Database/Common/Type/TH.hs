{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Database.Common.Type.TH where

import Data.Int
import Data.Char
import Data.ByteString qualified as BS
import Language.Haskell.TH

mkDataE :: String -> DecQ
mkDataE abc = dataD (pure []) (mkName "E") [] Nothing [
	recC (mkName "E") $ [
		varType "uuidV7High" ''Int64,
		varType "uuidV7Low" ''Int64,
		varType "idnt" ''BS.ByteString,
		varType "pubkey" ''BS.ByteString,
		varType "created_at" ''Int,
		varType "kind" ''Int
		] ++ ((`varType` ''Int64) <$> concat (hl . checkUpper <$> abc)) ++ [
		varType "tags" ''String,
		varType "content" ''String,
		varType "sig" ''BS.ByteString,
		varType "verified" ''Bool
		]
	] [derivClause Nothing [conT ''Show]]

hl :: String -> [String]
hl c = [c ++ "h", c ++ "l"]

checkUpper :: Char -> String
checkUpper = \case c | isUpper c -> 'u' : [toLower c] | otherwise -> [c]

varType :: String -> Name -> VarBangTypeQ
varType nm tp = varBangType (mkName nm) $ bangType noBang (conT tp)

noBang :: BangQ
noBang = bang noSourceUnpackedness noSourceStrictness
