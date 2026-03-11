{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Database.Common.HL (mkETags, hl) where

import Language.Haskell.TH

hl :: String -> [String]
hl c = [c ++ "h", c ++ "l"]

mkETags :: [String] -> [Name] -> [Q (Name, Exp)]
mkETags = (concat .) . zipWith (uncurry mkETag1) . (mkTagName <$>)

mkTagName :: String -> (Name, Name)
mkTagName c = (mkName $ c ++ "h", mkName $ c ++ "l")

mkETag1 :: Name -> Name -> Name -> [Q (Name, Exp)]
mkETag1 ah al ua = [
	(ah ,) <$> varE 'fst `appE` varE ua,
	(al ,) <$> varE 'snd `appE` varE ua ]
