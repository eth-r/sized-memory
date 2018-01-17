{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.ByteArray.Sized.Nat
  ( module GHC.TypeLits
  , ByteSize(..)
  , type Between
  , type AtLeast
  , type AtMost
  , type KnownNats
  , theNat
  ) where

import GHC.Num
import GHC.TypeLits

import Foundation

data ByteSize (n :: Nat) = ByteSize

type Between (atLeast :: Nat) (atMost :: Nat) (n :: Nat) =
  (KnownNat n, KnownNats atLeast atMost, atLeast <= n, n <= atMost)

type AtLeast (atLeast :: Nat) (n :: Nat) =
  (KnownNats n atLeast, atLeast <= n)

type AtMost (atMost :: Nat) (n :: Nat) =
  (KnownNats n atMost, n <= atMost)

type KnownNats (n :: Nat) (m :: Nat) =
  (KnownNat n, KnownNat m)

theNat :: forall n w. (KnownNat n, Num w) => w
theNat = fromIntegral $ natVal (ByteSize @n)
