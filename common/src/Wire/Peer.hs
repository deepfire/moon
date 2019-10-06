{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
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
module Wire.Peer
  ( Server(..)
  , runServer
  , ClientState(..)
  , runClient
  )
where

import qualified Data.ByteString.Lazy             as LBS

import           Codec.Serialise
import           Control.Tracer

import qualified Network.TypedProtocol.Channel    as Net
import           Network.TypedProtocol.Codec.Cbor hiding (decode, encode)
import           Network.TypedProtocol.Core         (Peer(..))
import qualified Network.TypedProtocol.Core       as Net
import qualified Network.TypedProtocol.Driver     as Net

import Wire.Protocol


data Server rej m a =
     Server
     { recvMsgRequest :: SomeRequest
                      -> m ( Either rej a
                           , Server rej m a )
     , recvMsgDone    :: ()
     }

runServer :: forall rej m a
           . (Monad m, Serialise rej, Show rej, m ~ IO, a ~ SomeReply)
          => Tracer m String
          -> Server rej m a
          -> Net.Channel IO LBS.ByteString
          -> IO ()
runServer tracer server channel = Net.runPeer (showTracing tracer) codec peerId channel peer
  where
    peerId = "client"
    peer   = serverPeer $ pure server

serverPeer :: forall rej m a
           . (m ~ IO, a ~ SomeReply)
           => m (Server rej m a)
           -> Peer (Piping rej) AsServer StIdle m ()
serverPeer server =
    Effect $ go <$> server
  where
    go :: Server rej m a
       -> Peer (Piping rej) AsServer StIdle m ()
    go Server{recvMsgRequest, recvMsgDone} =
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


data ClientState rej m a where
     ClientRequesting
       :: SomeRequest
       -> (Either rej a -> m (ClientState rej m a))
       -> ClientState rej m a

     ClientDone
       :: ClientState rej m a

runClient :: forall rej m a
           . (Monad m, Serialise rej, Show rej, a ~ SomeReply, m ~ IO)
          => Tracer m String
          -> m (ClientState rej m a)
          -> Net.Channel m LBS.ByteString
          -> m ()
runClient tracer firstStep channel = Net.runPeer (showTracing tracer) codec peerId channel peer
  where
    peerId = "server"
    peer   = mkClientSTS firstStep

mkClientSTS :: forall rej m a
           . (Monad m, a ~ SomeReply)
           => m (ClientState rej m a)
           -> Peer (Piping rej) AsClient StIdle m ()
mkClientSTS firstStep =
    Effect $ go <$> firstStep
  where
    go :: ClientState rej m a
       -> Peer (Piping rej) AsClient StIdle m ()
    go (ClientRequesting req csStep) =
      Yield (ClientAgency TokIdle) (MsgRequest req) $
        Await (ServerAgency TokBusy) $ \msg -> case msg of
          MsgReply rep ->
            Effect (go <$> csStep (Right rep))
          MsgBadRequest rej ->
            Effect (go <$> csStep (Left  rej))

    go ClientDone =
      Yield (ClientAgency TokIdle)
            MsgDone
            (Net.Done TokDone ())
