{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : JavaScript.WebSockets.Internal
-- Copyright   : (c) Justin Le 2015
-- License     : MIT
--
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : ghcjs
--
--
-- Low-level API for the 'Connection' socket wrapper, for situations like
-- debugging when things exported by "JavaScript.WebSockets" is not enough.
-- Most everyday usage should be covered by the aforementioned module, so
-- don't import this unless you really really have to.
--

module JavaScript.WebSockets.Internal (
  -- * Types
  -- ** Data types
    Connection(..)
  , ConnClosing(..)
  , Socket
  -- ** Exceptions
  , ConnectionException(..)
  -- * Manipulating and inspecting 'Connection's
  , openConnectionImmediate
  , closeConnection
  , closeConnectionLeftovers
  , clearConnectionQueue
  , dumpConnectionQueue
  , connectionClosed
  , connectionCloseReason
  , connectionStateCode
  -- * Sending and receiving
  , sendDadaDuda_
  , receiveMaybe
  -- * Connection mutex
  , withConnBlock
  , withConnBlockMasked
  ) where

import Prelude
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad             (void, join, when)
import Data.Binary               (Binary, encode, decodeOrFail)
import Data.ByteString           (ByteString)
import Data.IORef                (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe                (isJust, catMaybes)
import Data.Text as T            (Text, unpack, append)
import Data.Text.Encoding        (decodeUtf8', encodeUtf8, decodeUtf8)
import Data.Traversable          (mapM)
import Data.Typeable             (Typeable)
import JavaScript.Blob           (Blob, isBlob, readBlob)
import JavaScript.WebSockets.FFI
import Prelude hiding            (mapM)
import Unsafe.Coerce             (unsafeCoerce)

import qualified Data.ByteString.Base64      as B64
import qualified Data.ByteString.Base64.Lazy as B64L

#ifdef ghcjs_HOST_OS

import qualified JavaScript.Array as Array
import JavaScript.Array          (MutableJSArray(..))
import JavaScript.Object         (Object(..), setProp, getProp)
import qualified JavaScript.Object as Object
-- import JavaScript.TypedArray     (TypedArray(..))
-- import JavaScript.JSON.Types     (Value, Object) -- XXX: why oh why!?  ghcjs-base has it..
                                                    -- ..but also has not.
import GHCJS.DOM.Types           (ArrayBuffer, ToJSString(..), toJSString, fromJSString)
import GHCJS.Foreign             ( fromJSBool
                                 , jsTrue, jsNull, isUndefined)
import GHCJS.Marshal             (fromJSVal)
import qualified GHCJS.Prim as Prim
import GHCJS.Buffer
import GHCJS.Types               (JSVal, JSString, isNull)
#else
import JavaScript.NoGHCJS
#endif


#ifdef ghcjs_HOST_OS
foreign import javascript unsafe
  "$r = [];" js_emptyArray :: JSVal --Value
foreign import javascript unsafe
  "$r = {};" js_emptyObject :: Object
foreign import javascript unsafe "$1 === null"
  js_isNull :: JSVal -> Bool
foreign import javascript unsafe "$1 === undefined"
  js_isUndefined :: JSVal -> Bool

fromArray :: MutableJSArray -> IO [JSVal]
fromArray a = Prim.fromJSArray (unsafeCoerce a)
{-# INLINE fromArray #-}

emptyArray :: JSVal --Value
emptyArray = js_emptyArray
{-# INLINE emptyArray #-}

emptyObject :: Object
emptyObject = js_emptyObject
{-# INLINE emptyObject #-}

{- | Read a property from a JS object. Throws a JSException if
     o is not a JS object or the property cannot be accessed
 -}
getPropMaybe :: ToJSString a => a                -- ^ the property name
                             -> JSVal            -- ^ the object
                             -> IO (Maybe JSVal) -- ^ the property value, Nothing if the object doesn't have a property with the given name
getPropMaybe p o = do
  p' <- getProp (toJSString p) (unsafeCoerce o)
  if js_isUndefined p' then return Nothing else return (Just p')
{-# INLINE getPropMaybe #-}
#endif

-- | Encapsulates a (reference to a) Javascript Websocket connection.  Can
-- be created/accessed with either 'openConnection' or (preferably)
-- 'withUrl'.
--
-- Care must be taken to close the connection once you are done if using
-- 'openConnection', or unprocessed messages and callbacks will continue to
-- queue up.
--
data Connection = Connection { -- | JSRef to JS Websocket object
                               _connSocket     :: Socket
                               -- | JSRef to JSArray of queued incoming
                               -- messages, managed directly in FFI
                             , _connQueue      :: ConnectionQueue
                               -- | JSRef to JSArray of queue of waiting
                               -- receivers, managed directly in FFI
                             , _connWaiters    :: ConnectionWaiters
                               -- | Text of server socket was originally
                               -- opened with
                             , _connOrigin     :: Text
                               -- | IORef with Nothing if the connection is
                               -- still open and @Just reason@ if it's
                               -- closed, with the reason
                             , _connClosed     :: IORef (Maybe ConnClosing)
                               -- | Mutex for thread-safe manipulation
                             , _connBlock      :: MVar ()
                             }

-- | Data type containing information on 'Connection' closes.
--
-- *  'ManualClose': Closed by the Haskell 'JavaScript.WebSockets'
--    interface, using 'closeConnection' or variants.
--
-- *  'JSClose': Closed on the Javascript end, either by a connection error
--    or server request, or what have you.  Contains information from the
--    Javascript Websockets API
--    <http://www.w3.org/TR/websockets/#event-definitions>.
--
--    The first field is whether or not it was a clean close; the second
--    field is the closing reason code; the third field is a 'Text' with
--    the reason given by the Websockets API.
--
-- *  'OpenInterupptedClose': There was an unexpected error encountered
--    when attempting to open the connection.
--
-- *  'UnexpectedClose': Otherwise uncategorized closed status, with
--    a 'Text' field offering a reason.
data ConnClosing = ManualClose
                 | JSClose (Maybe Bool) (Maybe Int) (Maybe Text)
                 | OpenInterruptedClose
                 | UnexpectedClose Text
                 deriving (Show, Eq)

-- | An exception that may be thrown when using the various 'Connection'
-- operations.  Right now, only includes 'ConnectionClosed', which is
-- thrown when using an "unsafe" @receive@ on a closed 'Connection', or if
-- a 'Connection' closes while an unsafe @receive@ is waiting.
data ConnectionException = ConnectionClosed { _socketOrigin :: Text }
                         deriving (Eq, Typeable)
-- TODO: Other exceptions!  Open? Send?

instance Show ConnectionException where
    show (ConnectionClosed o) = T.unpack $ T.append "Error: Waiting on closed connection " o
instance Exception ConnectionException

-- | A version of 'openConnection' that doesn't wait for the connection to
-- be opened.  Returns an 'MVar' where the connection can be expected to be
-- placed when it is opened.
openConnectionImmediate :: Text -> IO (MVar Connection)
openConnectionImmediate url = do
    queue   <- Array.create
    waiters <- Array.create
    socket  <- ws_newSocket (toJSString url) queue waiters
    closed  <- newIORef Nothing
    block   <- newMVar ()
    let conn = Connection socket queue waiters url closed block
    outp    <- newEmptyMVar
    _       <- forkIO $ handleClose conn outp
    -- TODO: Opening errors
    _       <- forkIO $ handleOpen conn outp
    return outp

_dudConnection :: Text -> ConnClosing -> IO Connection
_dudConnection url closing = Connection jsNull
                         <$> Array.create
                         <*> Array.create
                         <*> pure url
                         <*> newIORef (Just closing)
                         <*> newMVar ()

handleOpen :: Connection -> MVar Connection -> IO ()
handleOpen conn connMVar =
    bracketOnError (return ())
                   (\_ -> _closeConnection OpenInterruptedClose False conn)
                  $ \_ -> do
                      _ <- ws_handleOpen (_connSocket conn)
                      putMVar connMVar conn

handleClose :: Connection -> MVar Connection -> IO ()
handleClose conn connMVar = do
    connState <- connectionStateCode conn
    when (connState < 3) . handle handler $ do
      closeEvent <- ws_handleClose (_connSocket conn)
      wasClean   <- fmap fromJSBool <$> getPropMaybe ("wasClean" :: Text) closeEvent
      code       <- fmap join . mapM fromJSVal =<< getPropMaybe ("code" :: Text) closeEvent
      reason     <- fmap unsafeCoerce <$> getPropMaybe ("reason" :: Text) closeEvent
      let jsClose = JSClose wasClean code reason
      _ <- _closeConnection jsClose False conn
      _ <- tryPutMVar connMVar conn
      return ()
  where
    -- TODO: any way to reasonably restore the handler?
    handler :: SomeAsyncException -> IO ()
    handler _ = void $ _closeConnection (UnexpectedClose reason) False conn
      where
        reason = "Close handler interrupted with Asynchronous Exception."

-- | Manually closes the given 'Connection'.  It un-blocks all threads
-- currently waiting on the connection and disables all sending and
-- receiving in the future.
--
-- The result is a list of all messages received by the connection but not
-- yet retrieved by 'receive', etc. on the Haskell end.
--
-- To close and ignore leftovers, use 'closeConnection'.
--
closeConnectionLeftovers :: Connection -> IO [ByteString]
closeConnectionLeftovers = _closeConnection ManualClose True

-- | Manually closes the given 'Connection'.  Will un-block all threads
-- currently waiting on the 'Connection' for messages (releasing their
-- callbacks) and disable sending and receiving in the future.
--
-- All leftover messages that were never processed on the Haskell end will
-- be deleted; use 'dumpConnectionQueue' to manually fetch them before
-- closing, or 'closeConnectionLeftovers' to recover them while closing.
closeConnection :: Connection -> IO ()
closeConnection = void . _closeConnection ManualClose False

_closeConnection :: ConnClosing -> Bool -> Connection -> IO [ByteString]
_closeConnection cclsing dump conn = withConnBlockMasked conn $ do
    closed <- isJust <$> readIORef (_connClosed conn)
    connState <- _connectionStateCode conn
    if closed || connState < 3
      then
        return []
      else do
        writeIORef (_connClosed conn) (Just cclsing)
        ws_closeSocket (_connSocket conn)
        ws_clearWaiters (_connWaiters conn)
        outp <- if dump
          then _dumpConnectionQueue conn
          else [] <$ ws_clearQueue (_connQueue conn)
        return outp

-- | Clears the message queue (messages waiting to be 'receive'd) on the
-- given 'Connection'.  Is essentially a no-op on closed connections.
clearConnectionQueue :: Connection -> IO ()
clearConnectionQueue conn = withConnBlockMasked conn $ do
    closed <- isJust <$> readIORef (_connClosed conn)
    when closed $ ws_clearQueue (_connQueue conn)

-- | Returns all incoming messages received by the socket and queued for
-- retrieval using 'receive' functions.  Empties the queue.
dumpConnectionQueue :: Connection -> IO [ByteString]
dumpConnectionQueue conn = withConnBlockMasked conn $
    _dumpConnectionQueue conn

-- | Execute process with the connection mutex lock in effect.  Will wait
-- until the lock is released before starting, if lock was already in
-- place.
--
-- Will break almost every 'Connection' function if you run one while this
-- is in effect, because almost all of them require the lock to begin.
withConnBlock :: Connection -> IO a -> IO a
withConnBlock conn f = withMVar (_connBlock conn) (const f)

-- | Execute process with the connection mutex lock in effect, with
-- asynchronos exceptions masked (See "Control.Exception").  Will wait
-- until the lock is released before starting, if lock was already in
-- place.
--
-- Will break almost every 'Connection' function if you run one while this
-- is in effect, because almost all of them require the lock to begin.
withConnBlockMasked :: Connection -> IO a -> IO a
withConnBlockMasked conn f = withMVarMasked (_connBlock conn) (const f)

_dumpConnectionQueue :: Connection -> IO [ByteString]
_dumpConnectionQueue conn = do
    msgsRefs <- fromArray (_connQueue conn)
    let results  = catMaybes $ _loadJSMessage <$> msgsRefs
    ws_clearQueue (_connQueue conn)
    return results


-- | Check if the given 'Connection' is closed.  Returns a 'Bool'.  To
-- check *why* it was closed, see 'connectionCloseReason'.
connectionClosed :: Connection -> IO Bool
connectionClosed = fmap isJust . connectionCloseReason

-- | Returns @Nothing@ if the given 'Connection' is still open, or @Just
-- closing@ containing a 'ConnClosing' with information on why the
-- connection was closed.
--
-- For just a 'Bool' saying whether or not the connection is closed, try
-- 'connectionClosed'.
connectionCloseReason :: Connection -> IO (Maybe ConnClosing)
connectionCloseReason conn = withConnBlock conn $
    readIORef (_connClosed conn)

-- | Returns the "readyState" of the connection's javascript websockets
-- API: 0 is connecting, 1 is open, 2 is closing, and 3 is closed.
-- Shouldn't really be used except for debugging purposes.  Use
-- 'connectionCloseReason' whenever possible to get information in a nice
-- haskelley sum type.
connectionStateCode :: Connection -> IO Int
connectionStateCode conn = withConnBlock conn $
    _connectionStateCode conn

_connectionStateCode :: Connection -> IO Int
_connectionStateCode conn = ws_readyState (_connSocket conn)

-- | Sends the given 'ByteString' through the given 'Connection'.
--
-- Returns 'True' if the connection is open, and 'False' if it is closed.
-- In the future will return more feedback about whether or not the send
-- was completed succesfully.
sendDadaDuda_ :: Connection -> ByteString -> IO Bool
sendDadaDuda_ conn msg = do
  closed <- connectionClosed conn
  if closed
    then
      return False
    else do
      -- TODO: Validate send here
      let (js_getArrayBuffer -> buf, start, end) = fromByteString msg
          sliced = js_sliceBuffer start end buf
      ws_socketSend (_connSocket conn) sliced
      return True
{-# NOINLINE sendDadaDuda_ #-}

foreign import javascript safe "$3.slice($1, $2)"
  js_sliceBuffer :: Int -> Int -> ArrayBuffer -> ArrayBuffer

foreign import javascript unsafe
  "$1.buf" js_getArrayBuffer    :: Buffer -> ArrayBuffer

-- | Block and wait until the 'Connection' receives any 'ByteString'.
--
-- Will return 'Just msg' as soon as any message is received, or 'Nothing'
-- if the 'Connection' closes first.  Returns 'Nothing' immediately if the
-- 'Connection' is already closed.
receiveMaybe :: Connection -> IO (Maybe ByteString)
receiveMaybe conn = do
  closed <- connectionClosed conn
  if closed
    then return Nothing
    else do
      waiterKilled <- Object.create
              -- set to ignore waiter if thread has died
      msg <- ws_awaitConn (_connQueue conn) (_connWaiters conn) (unsafeCoerce waiterKilled)
              `onException` setProp ("k" :: JSString) jsTrue waiterKilled
      pure $ _loadJSMessage (unsafeCoerce msg)

_loadJSMessage :: JSVal -> Maybe ByteString
_loadJSMessage msg | isNull (unsafeCoerce msg) = Nothing
                   | otherwise  =
      Just $ toByteString 0 Nothing (createFromArrayBuffer $ unsafeCoerce msg)

foreign import javascript unsafe
  "h$wrapBuffer" js_wrapBuffer :: ArrayBuffer -> Buffer
