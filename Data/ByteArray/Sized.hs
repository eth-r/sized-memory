{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK hide, show-extensions #-}
{-|
Module      : Data.ByteArray.Sized
Description : Sized byte arrays
Copyright   : (c) Promethea Raschke 2018
License     : public domain
Maintainer  : eth.raschke@liminal.ai
Stability   : experimental
Portability : unknown
-}
module Data.ByteArray.Sized
  ( module Data.ByteArray.Sized.Nat
  , ByteOp
  , ByteOps

  , Sized
  , unSized

  , empty
  , allocRet
  , singleton
  , replicate
  , zero

  , copy
  , copy'

  , asSized
  , coerce
  , convert

  , allZeros
  , set

  , append
  , append3

  , take

  , drop

  , tail

  , split
  , split3

  , xor
  ) where

-- import qualified Foundation as F
import Foundation
  ( (+)
  , (-)
  )
import Basement.Compat.Base
-- import Basement.Numerical.Multiplicative

-- import Data.Bits

import Data.ByteArray
  ( ByteArray
  , ByteArrayAccess
  )
import qualified Data.ByteArray as B
-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString as BS
import Control.DeepSeq
import Data.ByteArray.Sized.Nat

{-|
Convenience wrapper for the common constraint of byte array access in,
one byte array out
-}
type ByteOp a b = (ByteArrayAccess a, ByteArray b)

{-|
Convenience wrapper for the common constraint of two byte array accesses in,
one byte array out
-}
type ByteOps a b c = (ByteArrayAccess a, ByteArrayAccess b, ByteArray c)

{-|
A type for representing byte arrays whose length is known on the type level
-}
newtype ByteArrayAccess t => Sized (l :: Nat) t =
  Sized { unSized :: t } deriving (Eq, Ord, Show, ByteArrayAccess, NFData)

-- instance (ByteArray b) => ByteArrayAccess (Sized l b) where
--   length (Sized bs) = B.length bs
--   withByteArray (Sized bs) = B.withByteArray bs

{-- fun but dangerous for obscure memory reasons

instance (ByteArray b, KnownNat l) => Bits (Sized l b) where
  (.&.) = zipWithSized (.&.)
  (.|.) = zipWithSized (.|.)
  xor = xorN
  complement = mapSized complement
  shiftL (Sized bs) by = Sized $ B.convert $ shiftBitsL (bitth by) $
    shiftBytesL (byteth by) $ B.convert bs
  shiftR (Sized bs) by = Sized $ B.convert $ shiftBitsR (bitth by) $
    shiftBytesR (byteth by) $ B.convert bs
  rotateL (Sized bs) by = Sized $ B.convert $ rotateBitsL (bitth by) $
    rotateBytesL (byteth by) $ B.convert bs
  rotateR (Sized bs) by = Sized $ B.convert $ rotateBitsR (bitth by) $
    rotateBytesR (byteth by) $ B.convert bs
  bitSizeMaybe bs = Just $ 8 * B.length bs
  isSigned _ = False
  testBit (Sized bs) i = testBit (B.index bs $ byteth i) $ bitth i
  bit i = coerceToSized $ B.convert $ BS.snoc heads (bit i')
    where (ib, i') = bitByteth i
          heads = BS.replicate ib 0
  popCount (Sized bs) = BS.foldl' (\acc b -> acc + popCount b) 0 $ B.convert bs

bitth :: Int -> Int
bitth x = x `mod` 8

byteth :: Int -> Int
byteth x = x `div` 8

bitByteth :: Int -> (Int, Int)
bitByteth x = x `divMod` 8

shiftBytesL :: Int -> ByteString -> ByteString
shiftBytesL by bs
  | by >= blen = BS.replicate blen 0
  | by < 0 = shiftBytesR (-by) bs
  | otherwise = BS.append (BS.drop by bs) (BS.replicate by 0)
  where blen = BS.length bs

shiftBitsL :: Int -> ByteString -> ByteString
shiftBitsL by bs
  | by < 0 = shiftBitsR (-by) bs
  | otherwise = BS.pack $ BS.zipWith (.|.) shifted shifted'
  where shifted = BS.map (flip shiftL by') bs
        shifted' = shiftBytesL 1 $ BS.map (flip shiftL $ 8 - by') bs
        by' = bitth by

shiftBytesR :: Int -> ByteString -> ByteString
shiftBytesR by bs
  | by >= blen = BS.replicate blen 0
  | by < 0 = shiftBytesL (-by) bs
  | otherwise = BS.append (BS.replicate by 0) (BS.take (blen - by) bs)
  where blen = BS.length bs

shiftBitsR :: Int -> ByteString -> ByteString
shiftBitsR by bs
  | by < 0 = shiftBitsL (-by) bs
  | otherwise = BS.pack $ BS.zipWith (.|.) shifted shifted'
  where shifted = BS.map (flip shiftR by') bs
        shifted' = shiftBytesR 1 $ BS.map (flip shiftR $ 8 - by') bs
        by' = bitth by

rotateBytesL :: Int -> ByteString -> ByteString
rotateBytesL by bs =
  BS.append (BS.drop by' bs) (BS.take by' bs)
  where blen = BS.length bs
        by' = by `mod` blen

rotateBitsL :: Int -> ByteString -> ByteString
rotateBitsL by bs
  | by < 0 = rotateBitsR (-by) bs
  | otherwise = BS.pack $ BS.zipWith (.|.) rotated rotated'
  where rotated = BS.map (flip rotateL by') bs
        rotated' = rotateBytesL 1 $ BS.map (flip rotateL $ 8 - by') bs
        by' = bitth by

rotateBytesR :: Int -> ByteString -> ByteString
rotateBytesR by bs =
  BS.append (BS.drop by' bs) (BS.take by' bs)
  where blen = BS.length bs
        by' = blen - (by `mod` blen)

rotateBitsR :: Int -> ByteString -> ByteString
rotateBitsR by bs
  | by < 0 = rotateBitsL (-by) bs
  | otherwise = BS.pack $ BS.zipWith (.|.) rotated rotated'
  where rotated = BS.map (flip rotateR by') bs
        rotated' = rotateBytesR 1 $ BS.map (flip rotateR $ 8 - by') bs
        by' = bitth by

mapSized :: ByteArray a => (Word8 -> Word8) -> Sized l a -> Sized l a
mapSized f (Sized bs) = Sized $ B.convert $
  BS.map f (B.convert bs)

zipWithSized :: ByteOps a b c => (Word8 -> Word8 -> Word8) -> Sized l a -> Sized l b -> Sized l c
zipWithSized f (Sized as) (Sized bs) = Sized $ B.convert $
  BS.pack $ BS.zipWith f (B.convert as) (B.convert bs)

--}

{-|
Empty sized byte array
-}
empty :: ByteArray a => Sized 0 a
empty = Sized B.empty

{-|
Sized byte array of a single byte
-}
singleton :: ByteArray a => Word8 -> Sized 1 a
singleton x = Sized $ B.singleton x

{-|
Create sized byte array of specific byte
-}
replicate :: forall a x. (ByteArray a, KnownNat x) => Word8 -> Sized x a
replicate base = Sized $ B.replicate (theNat @x) base

{-|
Create sized byte array of zeros
-}
zero :: forall a x. (ByteArray a, KnownNat x) => Sized x a
zero = replicate 0

{-|
Allocate a sized byte array, run the initializer on it, and return
-}
allocRet :: forall a x p e. (ByteArray a, KnownNat x)
          => (Ptr p -> IO e) -> IO (e, Sized x a)
allocRet f = do
  let len = theNat @x
  (e, bs) <- B.allocRet len f
  return (e, Sized bs)

{-|
Copy a sized byte array, run the initializer on it, and return
-}
copy :: forall a x p. (ByteArray a, KnownNat x)
      => Sized x a -> (Ptr p -> IO ()) -> IO (Sized x a)
copy bs f = Sized <$> B.copy bs f

{-|
Copy a sized byte array, run the initializer on it, and return
-}
copy' :: forall a x p e. (ByteArray a, KnownNat x)
      => Sized x a -> (Ptr p -> IO e) -> IO (e, Sized x a)
copy' bs f = do
  (e, bs') <- B.copyRet bs f
  return (e, Sized bs')

{-|
Convert a byte array to a sized byte array

Returns 'Nothing' if the size doesn't match
-}
asSized :: forall a n. (ByteArray a, KnownNat n) => a -> Maybe (Sized n a)
asSized bs
  | B.length bs == theNat @n = Just (Sized bs)
  | otherwise = Nothing

{-|
Force a byte array to be a sized byte array

If the byte array is longer, it will be truncated to the correct length

If the byte array is shorter, it will be padded with zeros
-}
coerce :: forall a n. (ByteArray a, KnownNat n) => a -> Sized n a
coerce bs =
  case compare len x of
    EQ -> Sized bs
    GT -> Sized $ B.take x bs
    LT -> Sized $ B.append bs $ B.replicate (x - len) 0
  where
    len = B.length bs
    x = theNat @n

{-|
Convert between sized byte arrays of same length
-}
convert :: forall bin bout n. (ByteOp bin bout, KnownNat n) => Sized n bin -> Sized n bout
convert (Sized a) = Sized $ B.convert a

{-|
Test whether the sized byte array is only zeros
-}
allZeros :: ByteArray a => Sized x a -> Bool
allZeros (Sized a) = B.all (== 0) a

{-|
Set a byte of the sized byte array to a specific value
-}
set :: forall x a y. (ByteArray a, KnownNat x) => Sized (x + y) a -> Word8 -> Sized (x + y) a
set (Sized bs) x =
  Sized $ B.concat [heads, (B.singleton x), tails]
  where heads = B.take i bs
        tails = B.drop (i + 1) bs
        i = theNat @x

{-|
Append sized byte arrays
-}
append :: ByteArray a => Sized x a -> Sized y a -> Sized (x + y) a
append (Sized a) (Sized b) = Sized $ B.append a b

{-|
Append three sized byte arrays
-}
append3 :: ByteArray a => Sized x a -> Sized y a -> Sized z a -> Sized (x + (y + z)) a
append3 as bs cs = append as $ append bs cs

{-|
Take the correct number of bytes from the sized byte array
-}
take :: forall x a y. (ByteArray a, KnownNat x) => Sized (x + y) a -> Sized x a
take (Sized a) = Sized $ B.take (theNat @x) a

{-|
Drop the correct number of bytes from the sized byte array
-}
drop :: forall x a y. (ByteArray a, KnownNat x) => Sized (x + y) a -> Sized y a
drop (Sized a) = Sized $ B.drop (theNat @x) a

{-|
Take the correct number of bytes from the end of the sized byte array
-}
tail :: forall x a y. (ByteArray a, KnownNat y) => Sized (x + y) a -> Sized y a
tail (Sized a) = Sized $ B.drop (B.length a - theNat @y) a

{-|
Split the byte array at the correct length without a proxy argument
-}
split :: forall a x y. (ByteArray a, KnownNat x)
       => Sized (x + y) a -> (Sized x a, Sized y a)
split (Sized a) =
  let (b, c) = B.splitAt (theNat @x) a
  in (Sized b, Sized c)

{-|
Split the byte array to the correct lengths
-}
split3 :: forall a x y z. (ByteArray a, KnownNats x y, KnownNat (y + z))
        => Sized (x + (y + z)) a -> (Sized x a, Sized y a, Sized z a)
split3 as =
  let (a, bs) = split as
      (b, c) = split bs
  in (a, b, c)

{-|
Xor two sized byte arrays together
-}
xor :: ByteArray a => Sized x a -> Sized x a -> Sized x a
xor (Sized a) (Sized b) = Sized $ a `B.xor` b
