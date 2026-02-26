{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.Base16.Internal.SIMD
  ( c_isSIMDAvailable
  , c_encodeBase16SIMD
  , c_isValidBase16SIMD
  , c_decodeBase16SIMD
  , isValidBase16SIMD
  , decodeBase16SIMD
  , decodeBase16UntypedSIMD
  , encodeBase16SIMD
  ) where

import Foreign.C.Types
import Foreign.Ptr
import qualified Foreign.Marshal.Alloc  as Foreign
import Data.ByteString (StrictByteString)
import System.IO.Unsafe
import Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackMallocCStringLen)
import qualified Data.ByteString as BS

import Data.Base16.Types.Internal
import qualified Foreign.Marshal.Utils as Foreign
import Data.Text (Text)
import Data.Base16.Types (assertBase16)

foreign import capi "base16.h isSIMDAvailable"
  c_isSIMDAvailable :: Bool

foreign import capi "base16.h encodeBase16"
  c_encodeBase16SIMD
    :: Ptr CUChar
      -- ^ Buffer that holds the input value
    -> CSize
      -- ^ Length of the input
    -> Ptr CUChar
      -- ^ Buffer that will hold the encoded base16
    -> IO ()

foreign import capi "base16.h isValidBase16"
  c_isValidBase16SIMD
    :: Ptr CUChar
      -- ^ Buffer that holds the value to be validated
    -> CSize
      -- ^ Length of the input
    -> IO Bool

foreign import capi "base16.h decodeBase16"
  c_decodeBase16SIMD
    :: Ptr CUChar
      -- ^ Buffer that holds the input base16
    -> CSize
      -- ^ Length of the input
    -> Ptr CUChar
      -- ^ Buffer that will hold the decoded value
    -> IO ()

isValidBase16SIMD
  :: StrictByteString
  -> Bool
isValidBase16SIMD bytestring = unsafeDupablePerformIO $
  unsafeUseAsCStringLen bytestring $ \(cString, cStringLen) -> do
    c_isValidBase16SIMD (castPtr cString :: Ptr CUChar) (fromIntegral @Int @CSize cStringLen)

decodeBase16SIMD
  :: Base16 StrictByteString
  -> StrictByteString
decodeBase16SIMD (Base16 bytestring) = unsafeDupablePerformIO $
  unsafeUseAsCStringLen bytestring $ \(cString, cStringLen) -> do
    let outputLength :: Int = cStringLen `div` 2
    Foreign.allocaBytesAligned outputLength 32 $ \outputPtr -> do
      c_decodeBase16SIMD
        (castPtr @CChar @CUChar cString)
        (fromIntegral @Int @CSize cStringLen)
        outputPtr
      bsPtr <- Foreign.mallocBytes outputLength
      Foreign.copyBytes bsPtr outputPtr outputLength
      unsafePackMallocCStringLen (castPtr bsPtr :: Ptr CChar, fromIntegral outputLength)

decodeBase16UntypedSIMD
  :: StrictByteString
  -> Either Text StrictByteString
decodeBase16UntypedSIMD bytestring = do
  case BS.length bytestring `rem` 2 of
    0 ->
      if isValidBase16SIMD bytestring
        then Right $ decodeBase16SIMD $ assertBase16 bytestring
        else Left "invalid characters"
    _ -> Left "Size of input bytestring is not even"

encodeBase16SIMD
  :: StrictByteString
  -> Base16 StrictByteString
encodeBase16SIMD bytestring = unsafeDupablePerformIO $
  unsafeUseAsCStringLen bytestring $ \(cString, cStringLen) -> do
    let outputLength :: Int = cStringLen * 2
    Foreign.allocaBytesAligned outputLength 32 $ \outputPtr -> do
      c_encodeBase16SIMD
        (castPtr @CChar @CUChar cString)
        (fromIntegral @Int @CSize cStringLen)
        outputPtr
      bsPtr <- Foreign.mallocBytes outputLength
      Foreign.copyBytes bsPtr outputPtr outputLength
      assertBase16 <$> unsafePackMallocCStringLen (castPtr bsPtr :: Ptr CChar, fromIntegral outputLength)
