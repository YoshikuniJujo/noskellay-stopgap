{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Filter where

import Data.List qualified as L
import Data.Int
import Data.ByteString qualified as BS
import Data.Text qualified as T

data Value
	= Null | Integer Int64 | Real Double | Text T.Text | Blob BS.ByteString
	| MultiText [T.Text]
	deriving (Show, Eq, Ord)

data Comp = Eq | Ne | Lt | Gt | Le | Ge deriving Show

getComp :: Ord a => Comp -> a -> a -> Bool
getComp Eq = (==)
getComp Ne = (/=)
getComp Lt = (>)
getComp Gt = (<)
getComp Le = (>=)
getComp Ge = (<=)

data Filter e s
	= Never | Always
	| Atom Comp Value s
	| Filter e s `And` Filter e s
	| Filter e s `Or` Filter e s
	| Not (Filter e s)
	deriving Show

run :: (s -> e -> Value) -> Filter e s -> e -> Bool
run _ Never = const False
run _ Always = const True
run s (Atom Eq (Text t) gt) = \e -> case s gt e of
	Text t' -> t == t'
	MultiText ts' -> t `elem` ts'
	_ -> False
run s (Atom Eq (MultiText ts) gt) = \e -> case s gt e of
	Text t' -> t' `elem` ts
	MultiText ts' -> not . null $ ts `L.intersect` ts'
	_ -> False
run s (Atom cmp v gt) = getComp cmp v . s gt
run s (f1 `And` f2) = (&&) <$> run s f1 <*> run s f2
run s (f1 `Or` f2) = (||) <$> run s f1 <*> run s f2
run s (Not f) = not <$> run s f
