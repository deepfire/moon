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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}
module Moon.Protocol
  ( Request(..)
  , SomeRequest(..)
  , Reply(..)
  , SomeReply(..)
  , parseSomeRequest
  , Piping(..)
  , Protocol(..)
  , Message(..)
  , ClientHasAgency(..)
  , ServerHasAgency(..)
  , NobodyHasAgency(..)
  , codec
  )
where

import Debug.Trace
import Text.Printf

import qualified Algebra.Graph                    as G
import qualified Data.ByteString                  as  BS
import qualified Data.ByteString.Builder          as  BS
import qualified Data.ByteString.Builder.Extra    as  BS
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Lazy.Internal    as LBS (smallChunkSize)
import           Data.Kind                          (Type)
import           Data.List                          (intersperse)
import           Data.Proxy                         (Proxy(..))
import           Data.Text                          (Text)
import           Data.Typeable                      (Typeable)
import           GHC.Generics                       (Generic)
import           Options.Applicative
import           Text.Read
import           Type.Reflection
import qualified Type.Reflection.Unsafe           as Unsafe

import qualified Codec.CBOR.Decoding              as CBOR (Decoder,  decodeListLen, decodeWord)
import qualified Codec.CBOR.Encoding              as CBOR (Encoding, encodeListLen, encodeWord)
import qualified Codec.CBOR.Read                  as CBOR
import qualified Codec.CBOR.Write                 as CBOR
import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding
import           Control.Monad.ST                   (ST, stToIO)

import qualified Network.TypedProtocol.Channel    as Net
import qualified Network.TypedProtocol.Codec      as Codec
import           Network.TypedProtocol.Codec.Cbor hiding (decode, encode)
import           Network.TypedProtocol.Core         (Protocol(..))

import           Moon.Face hiding (Type)
import qualified Moon.Face as Face
import qualified Moon.Face.Ground as Ground

-- import Moon.Face.Haskell

--------------------------------------------------------------------------------
-- | Request/Reply:  asks with expectance of certain type of reply.
data Request (k :: Con) a
  = RunPipe PipeName [SomeValue]

data Reply   (k :: Con) a
  = ReplyValue SomeValue

instance Show (Request k a) where show (RunPipe n vs) = "RunPipe "    <> show n <> " " <> concat (intersperse " " (show <$> vs))
instance Show (Reply   k a) where show (ReplyValue n) = "ReplyValue " <> show n

--------------------------------------------------------------------------------
-- | SomeRequest/SomeReploy:  serialisable form of the above.
data SomeRequest = forall k a. SomeRequest (Request k a)

data SomeReply   = forall k a. SomeReply   (Reply   k a)

instance Show SomeRequest   where show (SomeRequest x) = show x
instance Show SomeReply     where show (SomeReply   x) = show x

parseSomeRequest :: Parser SomeRequest
parseSomeRequest = subparser $ mconcat
  [ cmd "RunPipe" $ SomeRequest
                      <$> (RunPipe
                             <$> (PipeName <$> opt "pipe")
                             <*> (many (opt "arg")))
  ]
  where
    cmd name p = command name $ info (p <**> helper) mempty
    opt name   = option auto (long name)

instance Read SomeValue where
  readPrec = do
    rtti@(Exists2 rtti') :: Some2 (RTTI2 Tag) <- readPrec
    case trace (printf "Got RTTI with tag/rep: %s, %s" (show $ Ground.rtti2Tag rtti') (show $ Ground.rtti2Rep rtti'))
         rtti of
      Exists2 x ->
        case x of
          (y :: RTTI2 Tag k b) -> do
            let tag' :: Tag' k = Ground.rtti2Tag y
                str            = Ground.rtti2Rep y
            case Ground.withGroundType str (readValue tag') of
              Nothing -> fail $ "Not a ground type: "<> show str
              Just x -> x
            where readValue :: forall (k :: Con). Tag' k -> Dict GroundContext -> ReadPrec SomeValue
                  readValue tag (Dict (p :: Proxy a)) = do
                    case tag of
                      TPoint' -> do
                        v :: a <- readPrec
                        pure $ SomeValue $ SomeKindValue $ mkValue' tag v
                      _ -> trace (printf "Can't Read values of any types, but Point.")
                                 (fail "")

--------------------------------------------------------------------------------
-- | Serialise instances

tagSomeRequest, tagSomeReply, tagSomeValue :: Word
tagSomeRequest = 31--415926535
tagSomeReply   = 27--182818284
tagSomeValue   = 16--180339887

instance Serialise SomeRequest where
  encode (SomeRequest x) = case x of
    RunPipe x vs -> encodeListLen 3 <> encodeWord tagSomeRequest <> encode x <> encode vs
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (3, tagSomeRequest) -> SomeRequest <$> (RunPipe <$> decode <*> decode)
      _ -> failLenTag len tag

instance Serialise SomeReply where
  encode (SomeReply x) = case x of
    ReplyValue x -> encodeListLen 2 <> encodeWord tagSomeReply <> encode (x :: SomeValue)
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (2, tagSomeReply) -> SomeReply <$> (ReplyValue <$> (decode :: Decoder s SomeValue))
      _ -> failLenTag len tag

failLenTag :: forall s a. Typeable a => Int -> Word -> Decoder s a
failLenTag len tag = fail $ "invalid "<>show (typeRep @a)<>" encoding: len="<>show len<>" tag="<>show tag

instance Serialise SomeValue where
  encode sv@(SomeValue x) =
    encodeListLen 3
    <> encodeWord tagSomeValue
    <> encode (someValueSomeTypeRep sv :: SomeTypeRep)
    <> encode x
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (3, tagSomeValue) -> do
        str :: SomeTypeRep <- decode
        case Ground.withGroundType str decodeSomeValue of
          Nothing -> fail $ "Not a ground type: "<> show str
          Just x -> x
        where decodeSomeValue :: Dict GroundContext -> Decoder s SomeValue
              decodeSomeValue (Dict (p :: Proxy a)) =
                SomeValue <$> (decode :: Decoder s (SomeKindValue a))

      _ -> failLenTag len tag

instance (Ord a, Typeable a, Serialise a) => Serialise (SomeKindValue a) where
  encode (SomeKindValue x) = case x of
    VPoint x -> encodeListLen 2 <> encodeWord 2 <> encode x
    VList  x -> encodeListLen 2 <> encodeWord 3 <> encode x
    VSet   x -> encodeListLen 2 <> encodeWord 4 <> encode x
    VTree  x -> encodeListLen 2 <> encodeWord 5 <> encode x
    VDag   x -> encodeListLen 2 <> encodeWord 6 <> encode x
    VGraph x -> encodeListLen 2 <> encodeWord 7 <> encode x
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (2, 2) -> SomeKindValue . VPoint <$> decode
      (2, 3) -> SomeKindValue . VList  <$> decode
      (2, 4) -> SomeKindValue . VSet   <$> decode
      (2, 5) -> SomeKindValue . VTree  <$> decode
      (2, 6) -> SomeKindValue . VDag   <$> decode
      (2, 7) -> SomeKindValue . VGraph <$> decode
      _ -> failLenTag len tag

--------------------------------------------------------------------------------
-- | Piping: protocol tag
data Piping rej
  = StIdle
  | StBusy
  | StDone
  deriving (Show)

instance Show (ClientHasAgency (st :: Piping rej)) where show TokIdle = "TokIdle"
instance Show (ServerHasAgency (st :: Piping rej)) where show TokBusy = "TokBusy"

--------------------------------------------------------------------------------
-- | Protocol & codec
instance Protocol (Piping rej) where
  data Message (Piping rej) from to where
    MsgRequest    :: SomeRequest  -> Message (Piping rej) StIdle StBusy
    MsgReply      :: SomeReply    -> Message (Piping rej) StBusy StIdle
    MsgBadRequest ::          rej -> Message (Piping rej) StBusy StIdle
    MsgDone       ::                 Message (Piping rej) StIdle StDone

  data ClientHasAgency st where TokIdle :: ClientHasAgency StIdle
  data ServerHasAgency st where TokBusy :: ServerHasAgency StBusy
  data NobodyHasAgency st where TokDone :: NobodyHasAgency StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}

deriving instance Show rej => Show (Message (Piping rej) from to)

codec :: forall rej. (Serialise rej) =>
  Codec (Piping rej) CBOR.DeserialiseFailure IO LBS.ByteString
codec =
    mkCodecCborLazyBS' enc dec
  where
    enc :: forall (pr :: PeerRole) st st'.
              PeerHasAgency pr st
           -> Message (Piping rej) st st'
           -> CBOR.Encoding
    enc (ClientAgency TokIdle) (MsgRequest r)    = CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> encode r
    enc (ServerAgency TokBusy) (MsgReply r)      = CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> encode r
    enc (ServerAgency TokBusy) (MsgBadRequest t) = CBOR.encodeListLen 2 <> CBOR.encodeWord 2 <> encode t
    enc (ClientAgency TokIdle)  MsgDone          = CBOR.encodeListLen 1 <> CBOR.encodeWord 3

    dec :: forall (pr :: PeerRole) s (st :: (Piping rej)).
              PeerHasAgency pr st
           -> CBOR.Decoder s (SomeMessage st)
    dec stok = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
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
          -> Message ps st st' -> CBOR.Encoding)

  -> (forall (pr :: PeerRole) (st :: ps) s.
             PeerHasAgency pr st
          -> CBOR.Decoder s (SomeMessage st))

  -> Codec ps CBOR.DeserialiseFailure m LBS.ByteString
mkCodecCborLazyBS'  cborMsgEncode cborMsgDecode =
    Codec {
      encode = \stok msg -> convertCborEncoder (cborMsgEncode stok) msg,
      decode = \stok     -> convertCborDecoder (cborMsgDecode stok)
    }
  where
    convertCborEncoder :: (a -> CBOR.Encoding) -> a -> LBS.ByteString
    convertCborEncoder cborEncode =
        toLazyByteString
      . CBOR.toBuilder
      . cborEncode

    convertCborDecoder
      :: (forall s. CBOR.Decoder s a)
      -> m (DecodeStep LBS.ByteString CBOR.DeserialiseFailure m a)
    convertCborDecoder cborDecode =
        (convertCborDecoderLBS cborDecode stToIO)

convertCborDecoderLBS
  :: forall s m a. Monad m
  => (CBOR.Decoder s a)
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
