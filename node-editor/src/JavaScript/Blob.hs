{-# LANGUAGE ForeignFunctionInterface, UnliftedFFITypes, JavaScriptFFI,
    UnboxedTuples, DeriveDataTypeable, GHCForeignImportPrim,
    MagicHash, FlexibleInstances, BangPatterns, Rank2Types, CPP #-}
{-# OPTIONS_HADDOCK hide #-}

module JavaScript.Blob (
    Blob
  , readBlob
  , isBlob
  , bufferByteString
  ) where

import Prelude
import           Control.Exception (mask_)
import           Data.ByteString (ByteString)
import           Data.ByteString.Unsafe (unsafePackAddressLen)
import           Data.Primitive.Addr (Addr(..))
import           Data.Primitive.ByteArray
import           Unsafe.Coerce (unsafeCoerce)

#ifdef ghcjs_HOST_OS
import GHCJS.Types       (JSVal)
--import GHCJS.DOM.Types
import GHCJS.DOM.JSFFI.Generated.Blob                 (Blob)
-- import GHCJS.DOM.JSFFI.Generated.FileReaderSync       (newFileReaderSync, readAsArrayBuffer)
#else
import JavaScript.NoGHCJS
#endif

#ifdef ghcjs_HOST_OS
foreign import javascript interruptible  "var reader = new FileReader();\
                                          reader.addEventListener('loadend', function() {\
                                            $c(reader.result);\
                                          });\
                                          reader.readAsArrayBuffer($1);"
  ffi_readBlob :: Blob -> IO JSVal

foreign import javascript unsafe "$1 instanceof Blob"
  ffi_blobCheck :: JSVal -> IO Bool

readBlob :: Blob -> IO ByteString
readBlob b = do
  -- reader <- newFileReaderSync
  -- abuff  <- readAsArrayBuffer reader b
  bufferByteString 0 0 =<< ffi_readBlob b

#else
ffi_readBlob :: Blob -> IO JSVal
ffi_blobCheck :: JSVal -> IO Bool

ffi_readBlob = error "ffi_readBlob: only available in JavaScript"
ffi_blobCheck = error "ffi_blobCheck: only available in JavaScript"
#endif

isBlob :: JSVal -> IO Bool
isBlob = ffi_blobCheck

--------------------------------------------------------------------------------
foreign import javascript safe "h$wrapBuffer($3, true, $1, $2)"
  js_wrapBuffer :: Int -> Int -> JSVal -> IO JSVal

{- | Convert an ArrayBuffer to a strict 'ByteString'
     this wraps the original buffer, without copying.
     Use 'byteArrayByteString' if you already have a wrapped buffer
 -}
bufferByteString :: Int        -- ^ offset from the start in bytes
                 -> Int        -- ^ length in bytes (use zero or a negative number to get the whole ArrayBuffer)
                 -> JSVal
                 -> IO ByteString
bufferByteString offset length buf = do
  (ByteArray ba) <- wrapBuffer offset length buf
  byteArrayByteString ba

{- | Convert a JavaScript ArrayBuffer to a 'ByteArray' without copying. Throws
     a 'JSException' if the 'JSVal' is not an ArrayBuffer.
 -}
wrapBuffer :: Int          -- ^ offset from the start in bytes, if this is not a multiple of 8,
                           --   not all types can be read from the ByteArray#
           -> Int          -- ^ length in bytes (use zero or a negative number to use the whole ArrayBuffer)
           -> JSVal        -- ^ JavaScript ArrayBuffer object
           -> IO ByteArray -- ^ result
wrapBuffer offset size buf = unsafeCoerce <$> js_wrapBuffer offset size buf
{-# INLINE wrapBuffer #-}

{- | Pack a ByteArray# primitive into a ByteString
     without copying the buffer.
     This is unsafe in native code
 -}
byteArrayByteString :: ByteArray#
                    -> IO ByteString
byteArrayByteString arr =
#ifdef ghcjs_HOST_OS
  let ba        = ByteArray arr
      !(Addr a) = byteArrayContents ba
  in  unsafePackAddressLen (sizeofByteArray ba) a
#else
  error "GHCJS.Foreign.byteArrayToByteString: not JS"
#endif
