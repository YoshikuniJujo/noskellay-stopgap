{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Nostr.Database.Filter where

import Prelude hiding (until)
import Foreign.C.Types
import Data.Char
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.UnixTime
import Crypto.Curve.Secp256k1

import "try-nostr-event-ng" Nostr.Event qualified as Event
import "try-nostr-event-ng" Nostr.Event.Signed qualified as Signed
import "try-nostr-event-ng" Nostr.Filter qualified as Filter

import Filter qualified as F

import Nostr.Value qualified as V

filterToFilter :: Filter.Filter -> F.Filter e Sel
filterToFilter f =
	maybe F.Always (foldr (F.Or . idToFilter) F.Never) (Filter.ids f) `F.And`
	maybe F.Always (foldr (F.Or . authorToFilter) F.Never) (Filter.authors f) `F.And`
	maybe F.Always (foldr (F.Or . kindToFilter) F.Never) (Filter.kinds f) `F.And`
	tagsToFilter (['a' .. 'z'] ++ ['A' .. 'Z']) f `F.And`
	maybe F.Always sinceToFilter (Filter.since f) `F.And`
	maybe F.Always untilToFilter (Filter.until f)

tagsToFilter :: [Char] -> Filter.Filter -> F.Filter e Sel
tagsToFilter ts f = foldr (F.And . flip tagToFilter' f) F.Always ts

tagToFilter' :: Char -> Filter.Filter -> F.Filter e Sel
tagToFilter' t f =
	maybe F.Always (foldr (F.Or . tagToFilter t) F.Never) (lookup t $ Filter.tags f)

tagToFilter :: Char -> T.Text -> F.Filter e Sel
tagToFilter t = flip (F.Atom F.Eq) (Tag t) . F.Text

data Sel = Id | Author | Kind | Tag Char | CreatedAt deriving Show

sel :: Sel -> Signed.E -> F.Value
sel Id = F.Blob . Signed.idnt
sel Author = F.Blob . BS.tail . serialize_point . Signed.pubkey
sel Kind = F.Integer . fromIntegral . Signed.kind
sel (Tag t) = F.MultiText . (fst <$>) . lookupAll (T.pack [t]) . Signed.tags
sel CreatedAt = F.Integer . (\(UnixTime (CTime t) _) -> t) . Signed.created_at

sel' :: Sel -> Event.E -> F.Value
sel' Id = F.Blob . Event.hash
sel' Author = F.Blob . BS.tail . serialize_point . Event.pubkey
sel' Kind = F.Integer . fromIntegral . Event.kind
sel' (Tag t) = F.MultiText . (fst <$>) . lookupAll (T.pack [t]) . Event.tags
sel' CreatedAt = F.Integer . (\(UnixTime (CTime t) _) -> t) . Event.created_at

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll a0 = \case
	[] -> []
	(x, y) : xs
		| x == a0 -> y : lookupAll a0 xs
		| otherwise -> lookupAll a0 xs

idToFilter :: BS.ByteString -> F.Filter e Sel
idToFilter = flip (F.Atom F.Eq) Id . F.Blob

authorToFilter ::Pub -> F.Filter e Sel
authorToFilter = flip (F.Atom F.Eq) Author
	. F.Blob . BS.tail . serialize_point

kindToFilter :: Int -> F.Filter e Sel
kindToFilter = flip (F.Atom F.Eq) Kind . F.Integer . fromIntegral

sinceToFilter :: UnixTime -> F.Filter e Sel
sinceToFilter = flip (F.Atom F.Ge) CreatedAt
	. F.Integer . (\(UnixTime (CTime t) _) -> t)

untilToFilter :: UnixTime -> F.Filter e Sel
untilToFilter = flip (F.Atom F.Le) CreatedAt
	. F.Integer . (\(UnixTime (CTime t) _) -> t)

sqlWhere :: (s -> String) -> F.Filter e s -> (String, [V.Value])
sqlWhere _ F.Never = ("false", [])
sqlWhere _ F.Always = ("true", [])
sqlWhere sts (F.Atom c v s) =
	(sts s ++ " " ++ compToSql c ++ " ?", [valueToValue v])
sqlWhere sts (f1 `F.And` f2) = let
	(s1, v1) = sqlWhere sts f1
	(s2, v2) = sqlWhere sts f2 in
	("(" ++ s1 ++ ") AND (" ++ s2 ++ ")", v1 ++ v2)
sqlWhere sts (f1 `F.Or` f2) = let
	(s1, v1) = sqlWhere sts f1
	(s2, v2) = sqlWhere sts f2 in
	("(" ++ s1 ++ ") OR (" ++ s2 ++ ")", v1 ++ v2)
sqlWhere sts (F.Not f) = let (s, v) = sqlWhere sts f in
	("NOT (" ++ s ++ ")", v)

valueToValue :: F.Value -> V.Value
valueToValue (F.Text t) = V.VText t
valueToValue _ = error "yet"

selToSql :: Sel -> String
selToSql Id = "id"
selToSql Author = "author"
selToSql Kind = "kind"
selToSql (Tag c)
	| isUpper c = "tags_u" ++ [toLower c] ++ ".value"
	| isLower c = "tags_" ++ [c] ++ ".value"
	| otherwise = error "bad"
selToSql CreatedAt = "created_at"

compToSql :: F.Comp -> String
compToSql F.Eq = "="
compToSql F.Ne = "<>"
compToSql F.Lt = "<"
compToSql F.Gt = ">"
compToSql F.Le = "<="
compToSql F.Ge = ">="
