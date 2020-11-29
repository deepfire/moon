{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Wire.Protocol
  ( Request(..)
  , Reply(..)
  , parseRequest
  , Piping(..)
  , Protocol(..)
  , Message(..)
  , ClientHasAgency(..)
  , ServerHasAgency(..)
  , NobodyHasAgency(..)
  , wireCodec
  )
where

import qualified Data.ByteString                  as  BS
import qualified Data.ByteString.Builder          as  BS
import qualified Data.ByteString.Builder.Extra    as  BS
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Lazy.Internal    as LBS (smallChunkSize)

import qualified Codec.CBOR.Read                  as CBOR
import qualified Codec.CBOR.Write                 as CBOR
import           Control.Monad.ST                   (ST, stToIO)

import qualified Network.TypedProtocol.Codec      as Codec
import           Network.TypedProtocol.Codec hiding (encode, decode)
import           Network.TypedProtocol.Core         (Protocol(..))

import Basis
import Dom.RequestReply


--------------------------------------------------------------------------------
-- | Piping:  message state type
--
data Piping rej
  = StIdle
  | StBusy
  | StDone
  deriving (Show)

instance Show (ClientHasAgency (st :: Piping rej)) where show TokIdle = "TokIdle"
instance Show (ServerHasAgency (st :: Piping rej)) where show TokBusy = "TokBusy"

--------------------------------------------------------------------------------
-- | Protocol & codec
--
instance Protocol (Piping rej) where
  data Message (Piping rej) from to where
    MsgRequest    :: StandardRequest -> Message (Piping rej) StIdle StBusy
    MsgReply      :: Reply           -> Message (Piping rej) StBusy StIdle
    MsgBadRequest ::       rej       -> Message (Piping rej) StBusy StIdle
    MsgDone       ::                    Message (Piping rej) StIdle StDone

  data ClientHasAgency st where TokIdle :: ClientHasAgency StIdle
  data ServerHasAgency st where TokBusy :: ServerHasAgency StBusy
  data NobodyHasAgency st where TokDone :: NobodyHasAgency StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}

deriving instance Show rej => Show (Message (Piping rej) from to)

wireCodec :: forall m rej. (Monad m, Serialise rej, m ~ IO) =>
  Codec (Piping rej) CBOR.DeserialiseFailure m LBS.ByteString
wireCodec =
    mkCodecCborLazyBS' enc dec
  where
    enc :: forall (pr :: PeerRole) st st'.
              PeerHasAgency pr st
           -> Message (Piping rej) st st'
           -> Encoding
    enc (ClientAgency TokIdle) (MsgRequest r)    = encodeListLen 2 <> encodeWord 0 <> encode r
    enc (ServerAgency TokBusy) (MsgReply r)      = encodeListLen 2 <> encodeWord 1 <> encode r
    enc (ServerAgency TokBusy) (MsgBadRequest t) = encodeListLen 2 <> encodeWord 2 <> encode t
    enc (ClientAgency TokIdle)  MsgDone          = encodeListLen 1 <> encodeWord 3

    dec :: forall (pr :: PeerRole) s (st :: (Piping rej)).
              PeerHasAgency pr st
           -> Decoder s (SomeMessage st)
    dec stok = do
      len <- decodeListLen
      key <- decodeWord
      case (stok, len, key) of
        (ClientAgency TokIdle, 2, 0) -> SomeMessage . MsgRequest    <$> decode
        (ServerAgency TokBusy, 2, 1) -> SomeMessage . MsgReply      <$> decode
        (ServerAgency TokBusy, 2, 2) -> SomeMessage . MsgBadRequest <$> decode
        (ClientAgency TokIdle, 1, 3) -> pure $ SomeMessage MsgDone

        (ClientAgency TokIdle, _, _) -> fail "codec.Idle: unexpected key"
        (ServerAgency TokBusy, _, _) -> fail "codec.Busy: unexpected key"

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
