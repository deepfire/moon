{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Wire.Protocol
  ( Request(..)
  , Reply(..)
  , parseRequest
  , Requests(..)
  , Replies(..)
  , Protocol(..)
  , Message(..)
  , ClientHasAgency(..)
  , ServerHasAgency(..)
  , NobodyHasAgency(..)
  , wireCodecRequests
  , wireCodecReplies
  )
where

import Data.ByteString                  qualified as  BS
import Data.ByteString.Builder          qualified as  BS
import Data.ByteString.Builder.Extra    qualified as  BS
import Data.ByteString.Lazy             qualified as LBS
import Data.ByteString.Lazy.Internal    qualified as LBS (smallChunkSize)

import Codec.CBOR.Read                  qualified as CBOR
import Codec.CBOR.Write                 qualified as CBOR
import Control.Monad.ST                             (ST, stToIO)

import Ouroboros.Network.Util.ShowProxy qualified as ONet

import Network.TypedProtocol.Codec      qualified as Codec
import Network.TypedProtocol.Codec           hiding (encode, decode)
import Network.TypedProtocol.Core                   (Protocol(..))

import Basis
import Dom.RequestReply


--------------------------------------------------------------------------------
-- | Piping:  message state type
--
data Requests rej
  = RqIdle
  | RqBusy
  | RqDone
  deriving (Show)

data Replies rej
  = ReIdle
  | ReBusy
  | ReDone
  deriving (Show)

instance ONet.ShowProxy (Replies rej) where
  showProxy _ = "Replies"

instance ONet.ShowProxy (Requests rej) where
  showProxy _ = "Requests"

instance Show (ClientHasAgency (st :: Requests rej)) where show RqIdleT = "RqIdleT"
instance Show (ServerHasAgency (st :: Requests rej)) where show RqBusyT = "RqBusyT"

instance Show (ClientHasAgency (st :: Replies rej)) where show ReIdleT = "ReIdleT"
instance Show (ServerHasAgency (st :: Replies rej)) where show ReBusyT = "ReBusyT"

--------------------------------------------------------------------------------
-- | Protocols & codecs
--
-- Note -- as of now, the protocols are really, _really_ trivial.
--
instance Protocol (Requests rej) where
  data Message (Requests rej) from to where
    MsgReq     :: StandardRequest ->   Message (Requests rej) RqIdle RqBusy
    MsgReqOk   ::                      Message (Requests rej) RqBusy RqIdle
    MsgReqDone ::                      Message (Requests rej) RqIdle RqDone

  data ClientHasAgency st where RqIdleT :: ClientHasAgency RqIdle
  data ServerHasAgency st where RqBusyT :: ServerHasAgency RqBusy
  data NobodyHasAgency st where RqDoneT :: NobodyHasAgency RqDone

  exclusionLemma_ClientAndServerHaveAgency RqIdleT tok = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency RqDoneT tok = case tok of {}
  exclusionLemma_NobodyAndServerHaveAgency RqDoneT tok = case tok of {}

deriving instance Show rej => Show (Message (Requests rej) from to)

instance Protocol (Replies rej) where
  data Message (Replies rej) from to where
    MsgReplyWait   ::                  Message (Replies rej) ReIdle ReBusy
    MsgReply       :: StandardReply -> Message (Replies rej) ReBusy ReIdle
    MsgReplyDone   ::                  Message (Replies rej) ReIdle ReDone

  data ClientHasAgency st where ReIdleT :: ClientHasAgency ReIdle
  data ServerHasAgency st where ReBusyT :: ServerHasAgency ReBusy
  data NobodyHasAgency st where ReDoneT :: NobodyHasAgency ReDone

  exclusionLemma_ClientAndServerHaveAgency ReIdleT tok = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency ReDoneT tok = case tok of {}
  exclusionLemma_NobodyAndServerHaveAgency ReDoneT tok = case tok of {}

deriving instance Show rej => Show (Message (Replies rej) from to)

wireCodecRequests :: forall m rej. (Monad m, Serialise rej, m ~ IO) =>
  Codec (Requests rej) CBOR.DeserialiseFailure m LBS.ByteString
wireCodecRequests =
    mkCodecCborLazyBS' enc dec
  where
    enc :: forall (pr :: PeerRole) st st'.
              PeerHasAgency pr st
           -> Message (Requests rej) st st'
           -> Encoding
    enc (ClientAgency RqIdleT) (MsgReq r)  = encodeListLen 2 <> encodeWord 0 <> encode r
    enc (ServerAgency RqBusyT)  MsgReqOk   = encodeListLen 1 <> encodeWord 1
    enc (ClientAgency RqIdleT)  MsgReqDone = encodeListLen 1 <> encodeWord 2

    dec :: forall (pr :: PeerRole) s (st :: (Requests rej)).
              PeerHasAgency pr st
           -> Decoder s (SomeMessage st)
    dec stok = do
      len <- decodeListLen
      key <- decodeWord
      case (stok, len, key) of
        (ClientAgency RqIdleT, 2, 0) -> fmap SomeMessage $ MsgReq <$> decode
        (ServerAgency RqBusyT, 1, 1) -> pure $ SomeMessage MsgReqOk
        (ClientAgency RqIdleT, 1, 2) -> pure $ SomeMessage MsgReqDone

        (ClientAgency RqIdleT, _, _) -> fail "codec.Idle: unexpected key"
        (ServerAgency RqBusyT, _, _) -> fail "codec.Busy: unexpected key"

wireCodecReplies :: forall m rej. (Monad m, Serialise rej, m ~ IO) =>
  Codec (Replies rej) CBOR.DeserialiseFailure m LBS.ByteString
wireCodecReplies =
    mkCodecCborLazyBS' enc dec
  where
    enc :: forall (pr :: PeerRole) st st'.
              PeerHasAgency pr st
           -> Message (Replies rej) st st'
           -> Encoding
    enc (ClientAgency ReIdleT)  MsgReplyWait = encodeListLen 1 <> encodeWord 0
    enc (ServerAgency ReBusyT) (MsgReply r)  = encodeListLen 2 <> encodeWord 1 <> encode r
    enc (ClientAgency ReIdleT)  MsgReplyDone = encodeListLen 1 <> encodeWord 2

    dec :: forall (pr :: PeerRole) s (st :: (Replies rej)).
              PeerHasAgency pr st
           -> Decoder s (SomeMessage st)
    dec stok = do
      len <- decodeListLen
      key <- decodeWord
      case (stok, len, key) of
        (ClientAgency ReIdleT, 1, 0) -> pure $ SomeMessage MsgReplyWait
        (ServerAgency ReBusyT, 2, 1) -> fmap SomeMessage $ MsgReply <$> decode
        (ClientAgency ReIdleT, 1, 2) -> pure $ SomeMessage MsgReplyDone

        (ClientAgency ReIdleT, _, _) -> fail "codec.Idle: unexpected key"
        (ServerAgency ReBusyT, _, _) -> fail "codec.Busy: unexpected key"

-- * Ancillary
--
mkCodecCborLazyBS'
  :: forall ps m
  . m ~ IO =>
  (forall (pr :: PeerRole) (st :: ps) (st' :: ps).
             PeerHasAgency pr st
          -> Message ps st st' -> Encoding)

  -> (forall (pr :: PeerRole) (st :: ps) s.
             PeerHasAgency pr st
          -> Decoder s (SomeMessage st))

  -> Codec ps CBOR.DeserialiseFailure m LBS.ByteString
mkCodecCborLazyBS'  cborMsgEncode cborMsgDecode =
    Codec {
      encode = \stok msg -> convertCborEncoder (cborMsgEncode stok) msg,
      decode = \stok     -> convertCborDecoder (cborMsgDecode stok)
    }
  where
    convertCborEncoder :: (a -> Encoding) -> a -> LBS.ByteString
    convertCborEncoder cborEncode =
        toLazyByteString
      . CBOR.toBuilder
      . cborEncode

    convertCborDecoder
      :: (forall s. Decoder s a)
      -> m (DecodeStep LBS.ByteString CBOR.DeserialiseFailure m a)
    convertCborDecoder cborDecode =
        convertCborDecoderLBS cborDecode stToIO

convertCborDecoderLBS
  :: forall s m a. Monad m
  => Decoder s a
  -> (forall b. ST s b -> m b)
  -> m (DecodeStep LBS.ByteString CBOR.DeserialiseFailure m a)
convertCborDecoderLBS cborDecode liftST =
    go [] =<< liftST (CBOR.deserialiseIncremental cborDecode)
  where
    -- Have to mediate between a CBOR decoder that consumes strict bytestrings
    -- and our choice here that consumes lazy bytestrings.
    go :: [BS.ByteString] -> CBOR.IDecode s a
       -> m (DecodeStep LBS.ByteString CBOR.DeserialiseFailure m a)
    go [] (CBOR.Done  trailing _ x)
      | BS.null trailing    = return (DecodeDone x Nothing)
      | otherwise           = return (DecodeDone x (Just trailing'))
                                where trailing' = LBS.fromStrict trailing
    go cs (CBOR.Done  trailing _ x) = return (DecodeDone x (Just trailing'))
                                where trailing' = LBS.fromChunks (trailing : cs)
    go _  (CBOR.Fail _ _ e) = return (DecodeFail e)

    -- We keep a bunch of chunks and supply the CBOR decoder with them
    -- until we run out, when we go get another bunch.
    go (c:cs) (CBOR.Partial  k) = go cs =<< liftST (k (Just c))
    go []     (CBOR.Partial  k) = return $ DecodePartial $ \mbs -> case mbs of
                                    Nothing -> go [] =<< liftST (k Nothing)
                                    Just bs -> go cs (CBOR.Partial k)
                                      where cs = LBS.toChunks bs

toLazyByteString :: BS.Builder -> LBS.ByteString
toLazyByteString = BS.toLazyByteStringWith strategy LBS.empty
  where
    strategy = BS.untrimmedStrategy 800 LBS.smallChunkSize
