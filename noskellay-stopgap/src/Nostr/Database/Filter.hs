{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Nostr.Database.Filter where

import Prelude hiding (until)
import Foreign.C.Types
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.UnixTime
import Crypto.Curve.Secp256k1

import "try-nostr-event-ng" Nostr.Event qualified as Event
import "try-nostr-event-ng" Nostr.Event.Signed qualified as Signed
import "try-nostr-event-ng" Nostr.Filter qualified as Filter

import Filter qualified as F

filterToFilter :: Filter.Filter -> F.Filter e Sel
filterToFilter f =
	maybe F.Always (foldr (F.Or . idToFilter) F.Never) (Filter.ids f) `F.And`
	maybe F.Always (foldr (F.Or . authorToFilter) F.Never) (Filter.authors f) `F.And`
	maybe F.Always (foldr (F.Or . kindToFilter) F.Never) (Filter.kinds f) `F.And`
	maybe F.Always (foldr (F.Or . tagToFilter 'a') F.Never) (lookup 'a' $ Filter.tags f) `F.And`
	maybe F.Always (foldr (F.Or . tagToFilter 'b') F.Never) (lookup 'b' $ Filter.tags f) `F.And`
	maybe F.Always (foldr (F.Or . tagToFilter 'c') F.Never) (lookup 'c' $ Filter.tags f) `F.And`
	maybe F.Always sinceToFilter (Filter.since f) `F.And`
	maybe F.Always sinceToFilter (Filter.until f)

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

tagToFilter :: Char -> T.Text -> F.Filter e Sel
tagToFilter t = flip (F.Atom F.Eq) (Tag t) . F.Text

sinceToFilter :: UnixTime -> F.Filter e Sel
sinceToFilter = flip (F.Atom F.Ge) CreatedAt
	. F.Integer . (\(UnixTime (CTime t) _) -> t)

untilToFilter :: UnixTime -> F.Filter e Sel
untilToFilter = flip (F.Atom F.Le) CreatedAt
	. F.Integer . (\(UnixTime (CTime t) _) -> t)
