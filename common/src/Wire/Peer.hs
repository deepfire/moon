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
  , runAsyncPeer
  , mkAsyncSubmitPeer
  , mkAsyncRequest
  , resumeAsyncPeer
  )
where

import qualified Data.ByteString.Lazy             as LBS

import           Codec.Serialise
import           Control.Tracer

import           Control.Monad.Class.MonadThrow

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Channel      (Channel(..))
import qualified Network.TypedProtocol.Channel    as Net
import           Network.TypedProtocol.Core         (Peer(..))
import qualified Network.TypedProtocol.Core       as Net
import qualified Network.TypedProtocol.Driver     as Net

import Basis
import Wire.Protocol


data Server rej m a =
     Server
     { processRequest :: SomeRequest
                      -> m ( Either rej a
                           , Server rej m a )
     , processDone    :: ()
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
    go Server{processRequest, processDone} =
      Await (ClientAgency TokIdle) $ \msg -> case msg of
        MsgRequest req ->
          Effect $ do
          (mrej, k) <- processRequest req
          pure $ case mrej of
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

        MsgDone ->
          Net.Done TokDone processDone


data ClientState rej m a where
     ClientRequesting
       :: SomeRequest
       -> (a -> m a)
       -> ClientState rej m a

     ClientProcessed
       :: a
       -> ClientState rej m a

     ClientAwaiting
       :: (TheyHaveAgency AsClient StBusy)
       -> (Message (Piping rej) StBusy StIdle
          -> Peer (Piping rej) AsClient StIdle m (ClientState rej m a))
       -> ClientState rej m a

     ClientDone
       :: ClientState rej m a

type CState rej m = ClientState rej m (Either rej SomeReply)

type CPeer st rej m a = Peer (Piping Text) AsClient st m a

runAsyncPeer
  :: forall ps (st :: ps) peerid failure bytes m a .
     (MonadThrow m, Exception failure)
  => Tracer m (Net.TraceSendRecv ps peerid failure)
  -> Codec ps failure m bytes
  -> peerid
  -> Channel m bytes
  -> Maybe bytes
  -> Peer ps AsClient st m a
  -> m a
runAsyncPeer tr Codec{encode, decode} peerid channel@Channel{send} mbytes =
    go mbytes
  where
    go :: forall st'.
          Maybe bytes
       -> Peer ps AsClient st' m a
       -> m a
    go  trailing (Effect     k) = k >>= go trailing
    go _trailing (Net.Done _ x) = return x
    go _trailing (Suspend  _ x) = return x
    go _trailing (Resume   _ x) = return x

    go trailing (Yield stok msg k) = do
      traceWith tr (Net.TraceSendMsg peerid (AnyMessage msg))
      send (encode stok msg)
      go trailing k

    go trailing (Await stok k) = do
      decoder <- decode stok
      res <- Net.runDecoderWithChannel channel trailing decoder
      case res of
        Right (SomeMessage msg, trailing') -> do
          traceWith tr (Net.TraceRecvMsg peerid (AnyMessage msg))
          go trailing' (k msg)
        Left failure ->
          throwM failure

resumeAsyncPeer
  :: forall rej m
   . (rej ~ Text, m ~ IO)
  => TheyHaveAgency AsClient StBusy
  -> CState rej m
  -> CPeer StBusy rej m (CState rej m)
resumeAsyncPeer _ (ClientAwaiting _ handler) =
  Await (ServerAgency TokBusy) $ \case
    r@MsgReply{}      -> handler r
    r@MsgBadRequest{} -> handler r

mkAsyncSubmitPeer
  :: forall rej m
   . (rej ~ Text, m ~ IO)
  => WeHaveAgency AsClient StIdle
  -> CState rej m
  -> CPeer StIdle rej m (CState rej m)
mkAsyncSubmitPeer tok state =
    Effect $ go <$> pure state
  where
    go :: CState rej m -> CPeer StIdle rej m (CState rej m)
    go (ClientRequesting req handleReply) =
      Yield (ClientAgency TokIdle) (MsgRequest req) $
        Suspend (ServerAgency TokBusy) $
          ClientAwaiting (ServerAgency TokBusy) $
            Effect . \case
              MsgReply rep ->
                Resume (ClientAgency TokIdle)
                  . ClientProcessed <$> handleReply (Right rep)
              MsgBadRequest rej ->
                Resume (ClientAgency TokIdle)
                  . ClientProcessed <$> handleReply (Left rej)
    go ClientDone =
      Yield (ClientAgency TokIdle)
            MsgDone
            (Net.Done TokDone ClientDone)

mkAsyncRequest
  :: forall rej m
  . (rej ~ Text, m ~ IO)
  => SomeRequest
  -> CState rej m
mkAsyncRequest req =
  ClientRequesting req handleReply
 where
   handleReply r@(Left rej) = do
     putStrLn $ "error: " <> unpack rej
     pure r
   handleReply r@(Right rep) = do
     putStrLn $ show rep
     pure r
