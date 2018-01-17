{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.ByteArray.Sized as Sized
import qualified Data.ByteArray as B
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Memory.PtrMethods

main :: IO ()
main = hspec $ do
  describe "Sized" $ do
    it "empty" $ do
      B.length (Sized.empty @ByteString) `shouldBe` 0
      unSized empty `shouldBe` ("" :: ByteString)

    prop "singleton" $ \x ->
      unSized (singleton x) `shouldBe` BS.singleton x

    prop "replicate" $ \x ->
      unSized (Sized.replicate @ByteString @32 x) `shouldBe` BS.replicate 32 x

    it "zero" $ do
      B.length (zero @ByteString @16) `shouldBe` 16

    it "copy" $ do
      let foo = coerce @ByteString @3 "foo"
      copied <- copy foo $ \p -> memSet p 0x67 1
      unSized copied `shouldBe` "goo"

    it "copy'" $ do
      let foo = coerce @ByteString @3 "foo"
      (e, copied) <- copy' foo $ \p -> memSet p 0x67 1 >> return 0
      unSized copied `shouldBe` "goo"
      e `shouldBe` 0

    it "allocRet" $ do
      (e, fff) <- allocRet $ \p -> memSet p 0x66 3 >> return 0
      e `shouldBe` 0
      fff `shouldBe` coerce @ByteString @3 "fff"

    it "asSized" $ do
      fmap unSized (asSized @ByteString @6 "foobar") `shouldBe` Just "foobar"
      fmap unSized (asSized @ByteString @8 "foobar") `shouldBe` Nothing

    it "coerce" $ do
      unSized (coerce @ByteString @6 "foobar") `shouldBe` "foobar"
      unSized (coerce @ByteString @3 "foobar") `shouldBe` "foo"
      unSized (coerce @ByteString @8 "foobar") `shouldBe` "foobar\x00\x00"

    it "convert" $ do
      unSized (convert @ByteString @B.Bytes
               (coerce @ByteString @6 "foobar"))
        `shouldBe` B.convert @ByteString "foobar"

    it "allZeros" $ do
      allZeros (coerce @ByteString @6 "foobar") `shouldBe` False
      allZeros (zero @ByteString @16) `shouldBe` True

    it "set" $ do
      let bs = coerce @ByteString @6 "foobar"
      unSized (set @2 bs 0x5a) `shouldBe` "foZbar"

    it "append" $ do
      let foo = coerce @ByteString @3 "foo"
          bar = coerce @ByteString @3 "bar"
      unSized (append foo bar) `shouldBe` "foobar"

    it "append3" $ do
      let foo = coerce @ByteString @3 "foo"
          bar = coerce @ByteString @3 "bar"
          baz = coerce @ByteString @3 "baz"
      unSized (append3 foo bar baz) `shouldBe` "foobarbaz"

    it "take" $ do
      let foobar = coerce @ByteString @6 "foobar"
      Sized.take foobar `shouldBe` coerce @ByteString @3 "foo"

    it "drop" $ do
      let foobar = coerce @ByteString @6 "foobar"
      Sized.drop foobar `shouldBe` coerce @ByteString @3 "bar"

    it "tail" $ do
      let foobar = coerce @ByteString @9 "foobarbaz"
      Sized.tail foobar `shouldBe` coerce @ByteString @3 "baz"

    it "split" $ do
      let foobar = coerce @ByteString @6 "foobar"
      let foo = coerce @ByteString @3 "foo"
          bar = coerce @ByteString @3 "bar"
      split foobar `shouldBe` (foo, bar)

    it "split3" $ do
      let foobarbaz = coerce @ByteString @12 "foobarbazgux"
      let foo = coerce @ByteString @3 "foo"
          barbaz = coerce @ByteString @6 "barbaz"
          gux = coerce @ByteString @3 "gux"
      split3 foobarbaz `shouldBe` (foo, barbaz, gux)

    it "xor" $ do
      let bar = coerce @ByteString @3 "bar"
          baz = coerce @ByteString @3 "baz"
      unSized (xor bar baz) `shouldBe` "\x00\x00\x08"
