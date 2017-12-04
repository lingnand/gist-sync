{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
module Utils.Functor
  (
    fmapLift

  , module Data.Vinyl.Functor
  ) where

import Data.Semigroup
import Data.Vinyl (Rec(..))
import Data.Vinyl.Functor
import Utils.Newtype

instance Newtype (Compose f g a) (f (g a)) where
  pack = Compose
  unpack = getCompose

fmapLift
  :: Functor h
  => Lift (->) f g a
  -> Lift (->) (h :. f) (h :. g) a
fmapLift (Lift f) = Lift $ over' (fmap f)

instance Monoid (f (g a)) => Monoid (Compose f g a) where
  mempty = Compose mempty
  Compose x `mappend` Compose y = Compose $ x `mappend` y

instance Semigroup (f (g a)) => Semigroup (Compose f g a) where
  Compose x <> Compose y = Compose $ x <> y

instance Semigroup (Rec f '[]) where
  RNil <> RNil = RNil

instance (Semigroup (f r), Semigroup (Rec f rs)) => Semigroup (Rec f (r ': rs)) where
  (x :& xs) <> (y :& ys) = (x <> y) :& (xs <> ys)
