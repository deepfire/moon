{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
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
import           Options.Applicative hiding (Parser)
import           Options.Applicative.Arrows
import qualified Options.Applicative              as Opt
import           Type.Reflection

import qualified Codec.CBOR.Decoding              as CBOR (Decoder,  decodeListLen, decodeWord)
import qualified Codec.CBOR.Encoding              as CBOR (Encoding, encodeListLen, encodeWord)
import qualified Codec.CBOR.Read                  as CBOR
import qualified Codec.CBOR.Write                 as CBOR
import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding
import           Control.Monad.ST                   (ST, stToIO)

import qualified Network.TypedProtocol.Codec      as Codec
import           Network.TypedProtocol.Codec  hiding (encode, decode)
-- import           Network.TypedProtocol.Codec.Cbor hiding (decode, encode)
import           Network.TypedProtocol.Core         (Protocol(..))

import Basis
import Ground (withRepGroundType, groundTypeReps)
import Pipe

--------------------------------------------------------------------------------
-- | Request/Reply:  asks with expectance of certain type of reply.
data Request
  = Run                  Text
  | Compile (QName Pipe) Text

data Reply
  = ReplyValue SomeValue

instance Show Request where
  show (Run       pipe) = "Run "                      <> show pipe
  show (Compile n text) = "Compile " <> show n <> " " <> unpack text
instance Show Reply where show (ReplyValue n) = "ReplyValue " <> show n

parseSomePipe :: (QName Pipe -> Maybe (SomePipe ())) -> Opt.Parser (SomePipe ())
parseSomePipe lookupPipe =
  runA $ proc () -> do
    p <- asA (strArgument (metavar "PIPEDESC")) -< ()
    returnA -< case do
      ast <- parse p
      compile opsDesc lookupPipe ast of
      Left  e -> error (unpack e)
      Right x -> x

parseRequest :: Opt.Parser Request
parseRequest = subparser $ mconcat
  [ cmd "run" $ Run <$> strArgument (metavar "PIPEDESC")
  , cmd "compile" $
    Compile
      <$> (QName <$> argument auto (metavar "NAME"))
      <*> strArgument (metavar "PIPEDESC")
  ]
  where
    cmd name p = command name $ info (p <**> helper) mempty

--------------------------------------------------------------------------------
-- | Serialise instances

tagRequest, tagReply, tagSomeValue :: Word
tagRequest   = 31--415926535
tagReply     = 27--182818284
tagSomeValue = 16--180339887

instance Serialise Request where
  encode x = case x of
    Run pipe -> encodeListLen 2 <> encodeWord (tagRequest + 0) <> encode pipe
    Compile nam expr -> encodeListLen 3 <> encodeWord (tagRequest + 1) <> encode nam <> encode expr
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (2, 31) -> Run     <$> decode
      (3, 32) -> Compile <$> decode <*> decode
      _ -> failLenTag len tag

instance Serialise Reply where
  encode x = case x of
    ReplyValue v -> encodeListLen 2 <> encodeWord tagReply <> encode (v :: SomeValue)
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (2, _tagSomeReply) -> ReplyValue <$> (decode :: Decoder s SomeValue)
      _ -> failLenTag len tag

failLenTag :: forall s a. Typeable a => Int -> Word -> Decoder s a
failLenTag len tag = fail $ "invalid "<>show (typeRep @a)<>" encoding: len="<>show len<>" tag="<>show tag

instance Serialise SomeValue where
  encode sv@(SomeValue k (SomeValueKinded x)) =
    encodeListLen 3
    <> encodeWord tagSomeValue
    <> encode (SomeCTag k)
    <> encode (someValueSomeTypeRep sv)
    <> encodeValue x
   where
     encodeValue :: Ground a => Value k a -> Encoding
     encodeValue = \case
       VPoint x -> encode x
       VList  x -> encode x
       VSet   x -> encode x
       VTree  x -> encode x
       VDag   x -> encode x
       VGraph x -> encode x
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    someCTag <- decode
    case (len, tag == tagSomeValue) of
      (3, True) -> do
        str :: SomeTypeRep <- decode
        case Ground.withRepGroundType str (decodeSomeValue someCTag) of
          Nothing -> fail $ mconcat
            ["Not a ground type: ", show str, "\n"
            ,"Ground types: ", show groundTypeReps]
          Just x -> x
        where decodeSomeValue ::
                SomeCTag -> TyDict Ground -> Decoder s SomeValue
              decodeSomeValue (SomeCTag ctag) (TyDict (a :: Proxy a)) =
                SomeValue
                  <$> pure ctag
                  <*> (SomeValueKinded <$> decodeValue a ctag)
              decodeValue :: forall s (k :: Con) a
                . Ground a => Proxy a -> CTag k -> Decoder s (Value k a)
              decodeValue _ = \case
                TPoint -> VPoint <$> decode
                TList  -> VList  <$> decode
                TSet   -> VSet   <$> decode
                TTree  -> VTree  <$> decode
                TDag   -> VDag   <$> decode
                TGraph -> VGraph <$> decode
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
    MsgRequest    :: Request  -> Message (Piping rej) StIdle StBusy
    MsgReply      :: Reply    -> Message (Piping rej) StBusy StIdle
    MsgBadRequest ::      rej -> Message (Piping rej) StBusy StIdle
    MsgDone       ::             Message (Piping rej) StIdle StDone

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
