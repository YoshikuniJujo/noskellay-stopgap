{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Mini.Database.Abc (abc, fromAbc, fromAbc') where

import Data.Char
import Data.Text qualified as T

abc :: String
abc = "abcdA"

fromAbc :: String -> [T.Text]
fromAbc [] = []
fromAbc (c : cs)
	| isUpper c = ("u" T.:> toLower c) : fromAbc cs
	| otherwise = ("" T.:> c) : fromAbc cs

fromAbc' :: String -> [String]
fromAbc' = \case
	"" -> []
	c : cs	| isUpper c -> ('u' : [toLower c]) : fromAbc' cs
		| otherwise -> [c] : fromAbc' cs
