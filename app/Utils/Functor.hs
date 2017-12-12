{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    (:.)

  , fmapLift
  , Uncurry(..)
  , Flip(..)
  , Fst(..)
  , Snd(..)

  , module Data.Functor.Compose
  , module Data.Vinyl.Functor
  ) where

import           Data.Functor.Compose
import           Data.Proxy
import           Data.Semigroup
import           Data.Vinyl (Rec(..))
import           Data.Vinyl.Functor (Lift(..), Const(..))
import           GHC.TypeLits
import           Utils.Newtype

infixr 9 :.
type f :. g = Compose f g

instance Newtype (Compose f g a) (f (g a)) where
  pack = Compose
  unpack = getCompose

instance Newtype (Const c a) c where
  pack = Const
  unpack = getConst

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


-- | Tuple type functions
data Uncurry f (tuple :: (a, b)) where
  Uncurry :: f a b -> Uncurry f '(a, b)

data Flip (f :: * -> * -> *) a b where
  Flip :: f b a -> Flip f a b

data Fst (tuple :: (a, b)) where
  Fst :: a -> Fst '(a, b)

instance (Show a, KnownSymbol b) => Show (Fst '(a, b)) where
  show (Fst v) =  show v ++ " <= " ++ symbolVal (Proxy :: Proxy b)

data Snd (tuple :: (a, b)) where
  Snd :: b -> Snd '(a, b)

instance (KnownSymbol a, Show b) => Show (Snd '(a, b)) where
  show (Snd v) =  symbolVal (Proxy :: Proxy a) ++ " => " ++ show v
