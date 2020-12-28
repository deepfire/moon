{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Wire.Peer
  ( RequestServer(..)
  , RequestClient(..)
  , wireCodecRequests
  , mkRequestServerSTS
  , mkRequestClientSTS
  , RepliesServer(..)
  , RepliesClient(..)
  , wireCodecReplies
  , mkRepliesServerSTS
  , mkRepliesClientSTS
  )
where

import Network.TypedProtocol.Codec
import Network.TypedProtocol.Core                      (Peer(..))
import Network.TypedProtocol.Core          qualified as Net

import Basis
import Dom.RequestReply
import Wire.Protocol


data RequestClient rej m a where
  ClientRequesting ::
       m StandardRequest
    -> RequestClient rej m a

  NoMoreRequests ::
       a
    -> RequestClient rej m a

data RequestServer rej m a where
  ServingRequest ::
       (StandardRequest
        -> m a)
    -> RequestServer rej m a

data RepliesServer rej m a where
  ServingReply ::
       m StandardReply
    -> RepliesServer rej m a

data RepliesClient rej m a where
  ClientReplyWait ::
       (StandardReply
        -> m a)
    -> RepliesClient rej m a

  NoMoreReplies ::
       a
    -> RepliesClient rej m a


mkRequestClientSTS :: forall rej m
           . (Monad m)
           => m (RequestClient rej m ())
           -> Peer (Requests rej) AsClient RqIdle m ()
mkRequestClientSTS firstStep =
  Effect $ go <$> firstStep
 where
   go :: RequestClient rej m a
      -> Peer (Requests rej) AsClient RqIdle m a

   go k@(ClientRequesting getNextReq) =
     Effect $ do
       req <- getNextReq
       pure $
         Yield (ClientAgency RqIdleT) (MsgReq req) $
           Await (ServerAgency RqBusyT) $ \case
             MsgReqOk -> go k

   go (NoMoreRequests retVal) =
     Yield (ClientAgency RqIdleT)
           MsgReqDone
           (Net.Done RqDoneT retVal)

mkRequestServerSTS :: forall rej m a
           . (Monad m)
           => m (RequestServer rej m a)
           -> Peer (Requests rej) AsServer RqIdle m ()
mkRequestServerSTS server =
    Effect $ go <$> server
  where
    go :: RequestServer rej m a
       -> Peer (Requests rej) AsServer RqIdle m ()
    go k@(ServingRequest submitRequest) =
      Await (ClientAgency RqIdleT) $ \case

        MsgReq req ->
          Effect $ do
            void $ submitRequest req -- The state is constant.
            pure $ Yield (ServerAgency RqBusyT) MsgReqOk (go k)

        MsgReqDone ->
          Net.Done RqDoneT ()


mkRepliesServerSTS :: forall rej m
           . (Monad m)
           => m (RepliesServer rej m StandardReply)
           -> Peer (Replies rej) AsServer ReIdle m ()
mkRepliesServerSTS server =
    Effect $ go <$> server
  where
    go :: RepliesServer rej m a
       -> Peer (Replies rej) AsServer ReIdle m ()

    go k@(ServingReply getNextReply) =
      Await (ClientAgency ReIdleT) $ \case

        MsgReplyWait ->
          Effect $ do
            r <- getNextReply
            pure $ Yield (ServerAgency ReBusyT) (MsgReply r) (go k)

        MsgReplyDone ->
          Net.Done ReDoneT ()

mkRepliesClientSTS :: forall rej m a
           . (Monad m)
           => m (RepliesClient rej m a)
           -> Peer (Replies rej) AsClient ReIdle m ()
mkRepliesClientSTS firstStep =
  Effect $ go <$> firstStep
 where
   go :: RepliesClient rej m a
      -> Peer (Replies rej) AsClient ReIdle m ()
   go k@(ClientReplyWait onReply) =

     Yield (ClientAgency ReIdleT) MsgReplyWait $
       Await (ServerAgency ReBusyT) $ \case
         MsgReply rep ->
           Effect $ do
             void $ onReply rep
             pure $ go k

   go (NoMoreReplies _retVal) =
     Yield (ClientAgency ReIdleT) MsgReplyDone $
       Net.Done ReDoneT ()
