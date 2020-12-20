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
  ( Server(..)
  , runServer
  , ClientState(..)
  , runClient
  , mkClientSTS
  -- , runAsyncPeer
  -- , mkAsyncSubmitPeer
  -- , mkAsyncRequest
  -- , resumeAsyncPeer
  )
where

import qualified Data.ByteString.Lazy                as LBS
import qualified Data.Text                           as T
import           Data.IntUnique

import           Codec.Serialise
import           Control.Tracer

import           Network.TypedProtocol.Codec
import qualified Network.TypedProtocol.Channel       as Net
import           Network.TypedProtocol.Core            (Peer(..))
import qualified Network.TypedProtocol.Core          as Net
import qualified Network.TypedProtocol.Driver.Simple as Net

import Basis (Text)
import Dom.RequestReply
import Wire.Protocol


data Server rej m a =
     Server
     { processRequest :: StandardRequest
                      -> m ( Either rej a
                           , Server rej m a )
     , processDone    :: ()
     }

runServer :: forall rej m a
           . (Monad m, Serialise rej, Show rej, m ~ IO, a ~ Reply)
          => Tracer m String
          -> Server rej m a
          -> Net.Channel IO LBS.ByteString
          -> IO ()
-- runPeer
-- :: forall ps (st :: ps) pr failure bytes m a
-- . (MonadThrow m, Exception failure)
-- => Tracer m (TraceSendRecv ps)
-- -> Codec ps failure m bytes
-- -> Channel m bytes
-- -> Peer ps pr st m a
-- -> m a Source #
runServer tracer server channel = Net.runPeer (showTracing tracer) wireCodec channel peer
  where
    peer   = serverPeer $ pure server

serverPeer :: forall rej m a
           . (m ~ IO, a ~ Reply)
           => m (Server rej m a)
           -> Peer (Piping rej) AsServer StIdle m ()
serverPeer server =
    Effect $ go <$> server
  where
    go :: Server rej m a
       -> Peer (Piping rej) AsServer StIdle m ()
    go Server{processRequest, processDone} =
      Await (ClientAgency TokIdle) $ \case
        MsgRequest uniq req ->
          Effect $ do
          (mrej, k) <- processRequest req
          pure $ case mrej of
            Right rep ->
              Yield
                (ServerAgency TokBusy)
                (MsgReply uniq rep)
                (go k)
            Left rej ->
              Yield
                (ServerAgency TokBusy)
                (MsgBadRequest uniq rej)
                (go k)

        MsgDone ->
          Net.Done TokDone processDone


data ClientState rej m a where
     ClientRequesting
       :: Unique
       -> StandardRequest
       -> (Unique -> Either rej a -> m (ClientState rej m a))
       -> ClientState rej m a

     ClientDone
       :: a
       -> ClientState rej m a

runClient :: forall rej m a
           . (Monad m, Serialise rej, Show rej, a ~ Reply, m ~ IO)
          => Tracer m Text
          -> Net.Channel m LBS.ByteString
          -> m (ClientState rej m a)
          -> m a
runClient tracer channel mkPeer =
  Net.runPeer (showTracingT tracer) wireCodec channel $
    mkClientSTS mkPeer
 where showTracingT :: forall a. Show a => Tracer IO Text -> Tracer IO a
       showTracingT tr = Tracer $ traceWith tr . T.pack . show

mkClientSTS :: forall rej m a
           . (Monad m, a ~ Reply)
           => m (ClientState rej m a)
           -> Peer (Piping rej) AsClient StIdle m a
mkClientSTS firstStep =
  Effect $ go <$> firstStep
 where
   go :: ClientState rej m a
      -> Peer (Piping rej) AsClient StIdle m a
   go (ClientRequesting reqUniq req csStep) =
     Yield (ClientAgency TokIdle) (MsgRequest reqUniq req) $
       Await (ServerAgency TokBusy) $ \case
         -- Note, that the reply does not necessarily correspond
         -- to the request.
         MsgReply replyUniq rep ->
           Effect (go <$> csStep replyUniq (Right rep))
         MsgBadRequest replyUniq rej ->
           Effect (go <$> csStep replyUniq (Left  rej))
   go (ClientDone retVal) =
     Yield (ClientAgency TokIdle)
           MsgDone
           (Net.Done TokDone retVal)
