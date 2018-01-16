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
  ( type ByteOp
  , type ByteOps

  , Sized(..)

  , emptyN
  , allocRetN
  , singletonN
  , replicateN
  , zerosN

  , copyN
  , copyN'

  , maybeToN
  , coerceToN
  , convertN

  , allZerosN
  , setN

  , appendN
  , appendN3

  , takeN'
  , takeN

  , dropN'
  , dropN

  , tailN'
  , tailN

  , splitN'
  , splitN
  , splitN3

  , xorN
  ) where

import Foundation
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
import Crypto.Lithium.Util.Nat

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
newtype ByteArrayAccess t =>
  Sized (l :: Nat) t =
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
replicate base = Sized $ B.replicate (asNum (ByteSize @x)) base

{-|
Create sized byte array of zeros
-}
zeros :: forall a x. (ByteArray a, KnownNat x) => Sized x a
zeros = replicateSized 0

{-|
Allocate a sized byte array, run the initializer on it, and return
-}
allocRet :: forall a x p e. (ByteArray a, KnownNat x)
          => (Ptr p -> IO e) -> IO (e, Sized x a)
allocRet f = do
  let len = asNum (ByteSize @x)
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
maybeToSized :: forall a n. (ByteArray a, KnownNat n) => a -> Maybe (Sized n a)
maybeToSized bs
  | B.length bs == asNum (ByteSize @n) = Just (Sized bs)
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
    x = asNum (ByteSize @n)

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
set :: (ByteArray a, KnownNat i) => proxy i -> Sized (i + (1 + x)) a -> Word8 -> Sized (i + (1 + x)) a
set proxy bs x =
  let (hd, tl) = splitN' proxy bs
      tl' = appendSized (singletonSized x) $ dropSized tl
  in appendSized hd tl'

{-|
Append sized byte arrays
-}
append :: ByteArray a => Sized x a -> Sized y a -> Sized (x + y) a
append (Sized a) (Sized b) = Sized $ B.append a b

{-|
Append three sized byte arrays
-}
append3 :: ByteArray a => Sized x a -> Sized y a -> Sized z a -> Sized (x + (y + z)) a
append3 as bs cs = appendSized as $ appendSized bs cs

{-|
Take 'proxy' number of bytes from the sized byte array
-}
take' :: (ByteArray a, KnownNat x) => proxy x -> Sized (x + y) a -> Sized x a
take' proxy (Sized a) = Sized $ B.take (asNum proxy) a

{-|
Take the correct number of bytes from the sized byte array
-}
take :: forall a x y. (ByteArray a, KnownNat x) => Sized (x + y) a -> Sized x a
take = take' (ByteSize @x)


{-|
Drop 'proxy' number of bytes from the sized byte array
-}
drop' :: (ByteArray a, KnownNat x) => proxy x -> Sized (x + y) a -> Sized y a
drop' proxy (Sized a) = Sized $ B.drop (asNum proxy) a

{-|
Drop the correct number of bytes from the sized byte array
-}
drop :: forall a x y. (ByteArray a, KnownNat x) => Sized (x + y) a -> Sized y a
drop = drop' (ByteSize @x)


{-|
Take /proxy/ number of bytes from the end of the sized byte array
-}
tail' :: (ByteArray a, KnownNat y) => proxy y -> Sized (x + y) a -> Sized y a
tail' proxy (Sized a) = Sized $ B.drop toDrop a
  where
    toDrop = B.length a - asNum proxy

{-|
Take the correct number of bytes from the end of the sized byte array
-}
tail :: forall a x y. (ByteArray a, KnownNat y) => Sized (x + y) a -> Sized y a
tail = tailN' (ByteSize @y)


{-|
Split the byte array at a length defined by 'proxy'
-}
split' :: (ByteArray a, KnownNat x)
        => proxy x -> Sized (x + y) a -> (Sized x a, Sized y a)
split' proxy (Sized a) =
  let (b, c) = B.splitAt (asNum proxy) a
  in (Sized b, Sized c)

{-|
Split the byte array at the correct length without a proxy argument
-}
split :: forall a x y. (ByteArray a, KnownNat x)
       => Sized (x + y) a -> (Sized x a, Sized y a)
split = split' (ByteSize @x)

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
