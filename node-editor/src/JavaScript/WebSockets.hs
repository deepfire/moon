-- |
-- Module      : JavaScript.WebSockets
-- Copyright   : (c) Justin Le 2015
-- License     : MIT
--
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : ghcjs
--
-- Contains functions and operations for working with Javascript Websocket
-- connections, which are encapsulated in the 'Connection' object.
--
-- It includes operations for opening, closing, inspecting connections and
-- operations for sending and receiving text and serializable data
-- (instances of 'Binary') through them.
--
-- Most of the necessary functionality is in hopefully in
-- "JavaScript.WebSockets"; more of the low-level API is exposed in
-- "JavaScript.WebSockets.Internal" if you need it for library
-- construction.
--

module JavaScript.WebSockets (
  -- * Usage
  -- $usage
  -- * Types
    Connection
  , ConnClosing(..)
  , ConnectionException(..)
  -- * Opening, closing, and working with connections
  , withUrl
  , withUrlLeftovers
  , openConnection
  , closeConnection
  , closeConnectionLeftovers
  , clearConnectionQueue
  , dumpConnectionQueue
  , connectionClosed
  , connectionCloseReason
  , connectionOrigin
  -- * Sending data
  , send
  -- , send_
  -- * Receiving data
  , receive
  , receiveMaybe
  ) where

import Prelude
import Control.Applicative
import Control.Concurrent             (readMVar)
import Control.Exception              (bracket, throw)
import Control.Monad                  (void)
import Data.ByteString                (ByteString)
import Data.Text                      (Text)
import JavaScript.WebSockets.Internal


-- $usage
--
-- > import Data.Text (unpack)
-- >
-- > -- A simple echo client, echoing all incoming text data
-- > main :: IO ()
-- > main = withUrl "ws://my-server.com" $ \conn ->
-- >     forever $ do
-- >         t <- receiveText conn
-- >         putStrLn (unpack t)
-- >         sendText conn t
--
-- The above code will attempt to interpret all incoming data as
-- UTF8-encoded Text, and throw away data that does not.
--
-- @conn@ is a 'Connection', which encapsulates a websocket channel.
--
-- You can also do the same thing to interpret all incoming data as any
-- instance of 'Binary' --- say, 'Int's:
--
-- > -- A simple client waiting for connections and outputting the running sum
-- > main :: IO ()
-- > main = withUrl "ws://my-server.com" (runningSum 0)
-- >
-- > runningSum :: Int -> Connection -> IO ()
-- > runningSum n conn = do
-- >     i <- receiveData conn
-- >     print (n + i)
-- >     runningSum (n + i) conn
--
-- 'receiveData' will block until the 'Connection' receives data that is
-- decodable as whatever type you expect, and will throw away all
-- nondecodable data (including 'Text' data).
--
-- The 'receive' function is provided as a over-indulgent layer of
-- abstraction where you can receive both 'Text' and instances of 'Binary'
-- with the same function using typeclass magic --- for the examples above,
-- you could use 'receive' in place of both 'receiveText' and
-- 'receiveData'.
--
-- 'send' works the same way for 'sendText' and 'sendData'.
--
-- If you want to, you can access the incoming data directly using the
-- 'SocketMsg' sum type, which exposes either a 'Text' or a lazy
-- 'ByteString':
--
-- > import Data.Text (unpack, append)
-- > import qualified Data.ByteString.Base64.Lazy as B64
-- >
-- > main :: IO ()
-- > main = withUrl "ws://my-server.com" $ \conn ->
-- >     forever $ do
-- >         msg <- receiveMessage
-- >         putStrLn $ case msg of
-- >             SocketMsgText t ->
-- >                 unpack $ append "Received text: " t
-- >             SocketMsgData d ->
-- >                 "Received data: " ++ show (B64.encode d)
--
-- You can talk to multiple connections by nesting 'withUrl':
--
-- > -- Act as a relay between two servers
-- > main :: IO ()
-- > main =  withUrl "ws://server-1.com" $ \conn1 ->
-- >         withUrl "ws://server-2.com" $ \conn2 ->
-- >             forever $ do
-- >                 msg <- receiveMessage conn1
-- >                 sendMessage conn2 msg
--
-- And also alternatively, you can manually open and close connections:
--
-- > -- Act as a relay between two servers
-- > main :: IO ()
-- > main = do
-- >     conn1 <- openConnection "ws://server-1.com"
-- >     conn2 <- openConnection "ws://server-2.com"
-- >     forever $ do
-- >         msg <- receiveMessage conn1
-- >         sendMessage conn2 msg
-- >     closeConnection conn2
-- >     closeConnection conn1
--
-- 'receiveMessage' and its varieties will all throw an exception if the
-- connection closes while they're waiting or if you attempt to receive on
-- a closed connection.  You can handle these with mechanisms from
-- "Control.Exception", or you can use their "maybe"-family counterparts,
-- 'receiveMessageMaybe', etc., who will return results in 'Just' on
-- a success, or return a 'Nothing' if the connection is closed or if
-- receiving on a closed connection.
--
-- You can use also @'connectionClosed' :: 'Connection' -> 'IO' 'Bool'@ to
-- check if the given 'Connection' object is closed (or
-- 'connectionCloseReason' to see *why*).
--
-- When closing connections, there might be some messages that were
-- received by the socket but never processed on the Haskell side with
-- a 'receive' method.  These will normally be deleted; however, you can
-- use 'closeConnectionLeftovers' or 'withUrlLeftovers' to grab a list of
-- the raw 'SocketMsg's remaining after closing.

-- | Like 'withUrl', except returns also the "leftover messages" that were
-- received by the socket but never processed on the Haskell end with
-- 'receive'.
--
withUrlLeftovers :: Text                  -- ^ Websocket address to connect to
                 -> (Connection -> IO a)  -- ^ Process to run on connection
                 -> IO (a, [ByteString])  -- ^ Result of process, with leftovers
withUrlLeftovers url process = withUrl url $ \conn ->
    liftA2 (,) (process conn) (dumpConnectionQueue conn)

-- | Performs the given @Connection -> IO a@ process attached to the given
-- server url.  Handles opening and closing the 'Connection' for you (and
-- clearing the message queue afterwards), and cleans up on errors.
--
-- If any messages were received by the socket but never processed/received
-- on the Haskell end, this will delete and drop them.  Use
-- 'withUrlLeftovers' to get a hold of them.
--
withUrl :: Text                   -- ^ Websocket address to connect to
        -> (Connection -> IO a)   -- ^ Process to run on connection
        -> IO a
withUrl url = bracket (openConnection url) closeConnection

-- | Opens a websocket connection to the given url, and returns the
-- 'Connection' after connection is completed and opened.  Care should be
-- taken to ensure that the 'Connection' is later closed with
-- 'closeConnection'.
--
-- Consider using 'withUrl', which handles closing with bracketing and
-- error handling so you don't have to worry about closing the connection
-- yourself.
--
-- Blocks until the connection has been established and opened.
--
-- If an async exception happens while this is waiting, the socket will be
-- closed as the exception bubbles up.
openConnection :: Text -> IO Connection
openConnection url = readMVar =<< openConnectionImmediate url

send :: Connection -> ByteString -> IO ()
send conn = void <$> sendDadaDuda_ conn

-- | Block and wait until the 'Connection' receives a "binary blob".  Throws a
-- 'ConnectionException' if the 'Connection' closes first.  Throws the exception
-- immediately if the 'Connection' is already closed and there are no queued
-- messages left.
--
-- To handle closed sockets with 'Maybe', use 'receiveMaybe'.
receive :: Connection -> IO ByteString
receive conn = do
  unjust <$> receiveMaybe conn
  where
    unjust (Just i ) = i
    unjust Nothing   = throw $ ConnectionClosed (_connOrigin conn)

-- | Returns the origin url of the given 'Connection'.
connectionOrigin :: Connection -> Text
connectionOrigin = _connOrigin
