{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.NG.Database.TH where

import Language.Haskell.TH

{-
columns :: [Name]
columns = beforeAtoZ ++
	(mkName <$> ["ah", "al", "bh", 
	-}

hl :: String -> [String]
hl c = [c ++ "h", c ++ "l"]
