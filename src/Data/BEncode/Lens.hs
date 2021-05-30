{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright :  (c) Joseph Morag 2021
-- License   :  BSD3
-- Maintainer:  Joseph Morag <jm@josephmorag.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module exports orphan instances for @'Ixed' 'BValue'@, @'Plated'
-- 'BValue'@, @'Ixed' 'BDictMap'@, @'Plated' 'BDictMap'@, @'Traversable'
-- 'BDictMap'@, @'FunctorWithIndex' 'BDictMap'@, @'FoldableWithIndex'
-- 'BDictMap'@, and @'TraversbaleWithIndex' 'BDictMap'@.
module Data.BEncode.Lens
  ( -- * Prisms
    _BInteger,
    _BString,
    _BList,
    _BDict,
   -- * BDicts and BLists
    members,
    key,
    nth,
    values,
  )
where

import Control.Lens
import Data.BEncode
import Data.BEncode.BDict as BE
import Data.BEncode.Types

_BInteger :: Prism' BValue BInteger
_BInteger = prism' BInteger (\case BInteger x -> Just x; _ -> Nothing)

_BString :: Prism' BValue BString
_BString = prism' BString (\case BString x -> Just x; _ -> Nothing)

_BList :: Prism' BValue BList
_BList = prism' BList (\case BList x -> Just x; _ -> Nothing)

_BDict :: Prism' BValue BDict
_BDict = prism' BDict (\case BDict x -> Just x; _ -> Nothing)

members :: IndexedTraversal' BKey BValue BValue
members = _BDict . itraversed

key :: BKey -> Traversal' BValue BValue
key k = _BDict . ix k

nth :: Int -> Traversal' BValue BValue
nth i = _BList . ix i

values :: IndexedTraversal' Int BValue BValue
values = _BList . traversed

------------------------------------------------------------------------------
-- Orphan instances for lens library interop
------------------------------------------------------------------------------
instance Traversable BDictMap where
  traverse _ Nil = pure Nil
  traverse f (Cons k x xs) = Cons k <$> f x <*> traverse f xs

instance FoldableWithIndex BKey BDictMap

instance FunctorWithIndex BKey BDictMap

instance TraversableWithIndex BKey BDictMap where
  itraverse _ Nil = pure Nil
  itraverse f (Cons k x xs) = Cons k <$> f k x <*> itraverse f xs

type instance Index (BDictMap a) = BKey

type instance IxValue (BDictMap a) = a

type instance Index BValue = BKey

type instance IxValue BValue = BValue

instance At (BDictMap a) where
  at :: BKey -> Lens' (BDictMap a) (Maybe a)
  at k f m =
    f mv <&> \case
      Nothing -> maybe m (const (delete k m)) mv
      Just v' -> insert k v' m
    where
      mv = BE.lookup k m
  {-# INLINE at #-}

-- | Insert a key value pair into a BDictMap. Overwrites the value for an
-- existing key
insert :: BKey -> a -> BDictMap a -> BDictMap a
insert k v Nil = BE.singleton k v
insert k v bd@(Cons k' x xs)
  | k == k' = Cons k v xs
  | k < k' = Cons k v bd
  | otherwise = Cons k' x (insert k v xs)

-- | Delete a key from a BDictMap. Returns the BDictMap unchanged if the key is
-- not present.
delete :: BKey -> BDictMap a -> BDictMap a
delete _ Nil = Nil
delete k bd@(Cons k' x xs)
  | k == k' = xs
  | k > k' = bd
  | otherwise = Cons k' x (delete k xs)

instance Ixed (BDictMap a)

instance Ixed BValue where
  ix i f (BDict o) = BDict <$> ix i f o
  ix _ _ v = pure v
  {-# INLINE ix #-}

instance Plated BValue where
  plate f (BDict o) = BDict <$> traverse f o
  plate f (BList l) = BList <$> traverse f l
  plate _ xs = pure xs
  {-# INLINE plate #-}