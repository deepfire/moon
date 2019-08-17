{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}
module Moon.Peer
  ( serverPeer
  , clientPeer
  , HaskellServer(..)
  , runServer
  , HaskellClient(..)
  , runClient
  , channelFromWebsocket)
where

import qualified Algebra.Graph                    as G
import qualified Data.ByteString                  as  BS
import qualified Data.ByteString.Builder          as  BS
import qualified Data.ByteString.Builder.Extra    as  BS
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Lazy.Internal    as LBS (smallChunkSize)
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)

import qualified Codec.CBOR.Decoding              as CBOR (Decoder,  decodeListLen, decodeWord)
import qualified Codec.CBOR.Encoding              as CBOR (Encoding, encodeListLen, encodeWord)
import qualified Codec.CBOR.Read                  as CBOR
import qualified Codec.CBOR.Write                 as CBOR
import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding
import           Control.Exception
import           Control.Monad.ST                   (ST, stToIO)
import           Control.Tracer

import qualified Network.TypedProtocol.Channel    as Net
import qualified Network.TypedProtocol.Codec      as Codec
import           Network.TypedProtocol.Codec.Cbor hiding (decode, encode)
import           Network.TypedProtocol.Core         (Peer(..), Protocol(..))
import qualified Network.TypedProtocol.Core       as Net
import qualified Network.TypedProtocol.Driver     as Net
import qualified Network.WebSockets               as WS

import Moon.Face
import Moon.Face.Haskell
import Moon.Protocol


data HaskellServer rej m a =
     HaskellServer
     { recvMsgRequest :: SomeHaskellRequest
                      -> m ( Either rej a
                           , HaskellServer rej m a )
     , recvMsgDone    :: ()
     }

runServer :: forall rej m a
           . (rej ~ Text, m ~ IO, a ~ SomeHaskellReply)
          => Tracer m String
          -> HaskellServer rej m a
          -> Net.Channel IO LBS.ByteString
          -> IO ()
runServer tracer server channel = Net.runPeer (showTracing tracer) codec peerId channel peer
  where
    codec  = codecHaskell
    peerId = "client"
    peer   = serverPeer $ pure server

serverPeer :: forall rej m a
           . (rej ~ Text, m ~ IO, a ~ SomeHaskellReply)
           => m (HaskellServer rej m a)
           -> Peer (Haskell rej) AsServer StIdle m ()
serverPeer server =
    Effect $ go <$> server
  where
    go :: HaskellServer rej m a
       -> Peer (Haskell rej) AsServer StIdle m ()
    go HaskellServer{recvMsgRequest, recvMsgDone} =
      Await (ClientAgency TokIdle) $ \msg -> case msg of
        MsgRequest req -> Effect $ do
          (mrej, k) <- recvMsgRequest req
          return $
            case mrej of
              Right rep ->
                Yield
                  (ServerAgency TokBusy)
                  (MsgReply rep)
                  (go k)
              Left rej ->
                Yield
                  (ServerAgency TokBusy)
                  (MsgBadRequest rej)
                  (go k)

        MsgDone -> Net.Done TokDone recvMsgDone


data HaskellClient rej m a where
     SendMsgRequest
       :: SomeHaskellRequest
       -> (Either rej a -> m (HaskellClient rej m a))
       -> HaskellClient rej m a

     SendMsgDone
       :: HaskellClient rej m a

runClient :: forall rej m a
           . (rej ~ Text, m ~ IO, a ~ SomeHaskellReply)
          => Tracer m String
          -> HaskellClient rej m a
          -> Net.Channel IO LBS.ByteString
          -> IO ()
runClient tracer client channel = Net.runPeer (showTracing tracer) codec peerId channel peer
  where
    codec  = codecHaskell
    peerId = "server"
    peer   = clientPeer $ pure client

clientPeer :: forall rej m a
           . (rej ~ Text, m ~ IO, a ~ SomeHaskellReply)
           => m (HaskellClient rej m a)
           -> Peer (Haskell rej) AsClient StIdle m ()
clientPeer client =
    Effect $ go <$> client
  where
    go :: HaskellClient rej m a
       -> Peer (Haskell rej) AsClient StIdle m ()
    go (SendMsgRequest req k) =
      Yield (ClientAgency TokIdle)
            (MsgRequest req) $
      Await (ServerAgency TokBusy) $ \msg -> case msg of
        MsgReply      rep -> Effect (go <$> k (Right rep))
        MsgBadRequest rej -> Effect (go <$> k (Left  rej))

    go SendMsgDone =
      Yield (ClientAgency TokIdle)
            MsgDone
            (Net.Done TokDone ())


channelFromWebsocket :: WS.Connection -> Net.Channel IO LBS.ByteString
channelFromWebsocket conn =
  Net.Channel
  { recv = catch (Just <$> WS.receiveData conn) $
           (\(SomeException x) -> pure Nothing)
  , send = WS.sendBinaryData conn
  }
