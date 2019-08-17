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
module Moon.Protocol
  ( Haskell(..)
  , Protocol(..)
  , Message(..)
  , ClientHasAgency(..)
  , ServerHasAgency(..)
  , NobodyHasAgency(..)
  , codecHaskell
  )
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
import           Control.Monad.ST                   (ST, stToIO)

import qualified Network.TypedProtocol.Channel    as Net
import qualified Network.TypedProtocol.Codec      as Codec
import           Network.TypedProtocol.Codec.Cbor hiding (decode, encode)
import           Network.TypedProtocol.Core         (Protocol(..))

import Moon.Face
import Moon.Face.Haskell

instance Serialise SomeHaskellRequest where
  encode (SomeHaskellRequest x) = case x of
    Indexes          -> encodeListLen 1 <> encodeWord 0
    PackageRepo  x y -> encodeListLen 3 <> encodeWord 1 <> encode x <> encode y
    RepoPackages   x -> encodeListLen 2 <> encodeWord 2 <> encode x
    PackageModules x -> encodeListLen 2 <> encodeWord 3 <> encode x
    ModuleDeps     x -> encodeListLen 2 <> encodeWord 4 <> encode x
    ModuleDefs     x -> encodeListLen 2 <> encodeWord 5 <> encode x
    DefLoc         x -> encodeListLen 2 <> encodeWord 6 <> encode x
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (1, 0) -> pure $ SomeHaskellRequest Indexes
      (3, 1) -> SomeHaskellRequest <$> (PackageRepo    <$> decode <*> decode)
      (2, 2) -> SomeHaskellRequest <$> (RepoPackages   <$> decode)
      (2, 3) -> SomeHaskellRequest <$> (PackageModules <$> decode)
      (2, 4) -> SomeHaskellRequest <$> (ModuleDeps     <$> decode)
      (2, 5) -> SomeHaskellRequest <$> (ModuleDefs     <$> decode)
      (2, 6) -> SomeHaskellRequest <$> (DefLoc         <$> decode)
      _      -> fail $ "invalid SomeHaskellRequest encoding: len="<>show len<>" tag="<>show tag

instance Serialise SomeHaskellReply where
  encode = \case
    PlyIndexes        x -> encodeListLen 2 <> encodeWord 0 <> encode (SomeReply x)
    PlyRepoURL        x -> encodeListLen 2 <> encodeWord 1 <> encode (SomeReply x)
    PlyRepoPackages   x -> encodeListLen 2 <> encodeWord 2 <> encode (SomeReply x)
    PlyPackageModules x -> encodeListLen 2 <> encodeWord 3 <> encode (SomeReply x)
    PlyModuleDeps     x -> encodeListLen 2 <> encodeWord 4 <> encode (SomeReply x)
    PlyModuleDefs     x -> encodeListLen 2 <> encodeWord 5 <> encode (SomeReply x)
    PlyDefLoc         x -> encodeListLen 2 <> encodeWord 6 <> encode (SomeReply x)
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (2, 0) -> (decode :: Decoder s (SomeReply Index))   >>= \case SomeReply x@(RList _)  -> pure (PlyIndexes        x)
                                                                    _                      -> fail "invalid PlyIndexes"
      (2, 1) -> (decode :: Decoder s (SomeReply URL))     >>= \case SomeReply x@(RPoint _) -> pure (PlyRepoURL        x)
                                                                    _                      -> fail "invalid PlyRepoURL"
      (2, 2) -> (decode :: Decoder s (SomeReply Package)) >>= \case SomeReply x@(RList _)  -> pure (PlyRepoPackages   x)
                                                                    _                      -> fail "invalid PlyRepoPackages"
      (2, 3) -> (decode :: Decoder s (SomeReply Module))  >>= \case SomeReply x@(RTree _)  -> pure (PlyPackageModules x)
                                                                    _                      -> fail "invalid PlyPackageModules"
      (2, 4) -> (decode :: Decoder s (SomeReply Package)) >>= \case SomeReply x@(RTree _)  -> pure (PlyModuleDeps     x)
                                                                    _                      -> fail "invalid PlyModuleDeps"
      (2, 5) -> (decode :: Decoder s (SomeReply Package)) >>= \case SomeReply x@(RList _)  -> pure (PlyModuleDefs     x)
                                                                    _                      -> fail "invalid PlyModuleDefs"
      (2, 6) -> (decode :: Decoder s (SomeReply Loc))     >>= \case SomeReply x@(RPoint _) -> pure (PlyDefLoc         x)
                                                                    _                      -> fail "invalid PlyDefLoc"
      _      -> fail $ "invalid SomeHaskellReply encoding: len="<>show len<>" tag="<>show tag

instance Serialise a => Serialise (SomeReply a) where
  encode (SomeReply x) = case x of
    RPoint x -> encodeListLen 2 <> encodeWord 0 <> encode x
    RList  x -> encodeListLen 2 <> encodeWord 1 <> encode x
    RTree  x -> encodeListLen 2 <> encodeWord 2 <> encode x
    RDag   x -> encodeListLen 2 <> encodeWord 3 <> encode x
    RGraph x -> encodeListLen 2 <> encodeWord 4 <> encode x
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (2, 0) -> SomeReply . RPoint <$> decode
      (2, 1) -> SomeReply . RList  <$> decode
      (2, 2) -> SomeReply . RTree  <$> decode
      (2, 3) -> SomeReply . RDag   <$> decode
      (2, 4) -> SomeReply . RGraph <$> decode
      _      -> fail $ "invalid SomeReply encoding: len="<>show len<>" tag="<>show tag

data Haskell rej
  = StIdle
  | StBusy
  | StDone

deriving instance Show rej => Show (Message (Haskell rej) from to)

instance Show (ClientHasAgency (st :: Haskell rej)) where
  show TokIdle = "TokIdle"

instance Show (ServerHasAgency (st :: Haskell rej)) where
  show TokBusy = "TokBusy"

instance Protocol (Haskell rej) where
  data Message (Haskell rej) from to where
    MsgRequest
      :: SomeHaskellRequest
      -> Message (Haskell rej) StIdle StBusy
    MsgReply
      :: SomeHaskellReply
      -> Message (Haskell rej) StBusy StIdle
    MsgBadRequest
      :: rej
      -> Message (Haskell rej) StBusy StIdle
    MsgDone
      :: Message (Haskell rej) StIdle StDone

  data ClientHasAgency st where TokIdle :: ClientHasAgency StIdle
  data ServerHasAgency st where TokBusy :: ServerHasAgency StBusy
  data NobodyHasAgency st where TokDone :: NobodyHasAgency StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}

codecHaskell :: forall rej. (Serialise rej) =>
  Codec (Haskell rej) CBOR.DeserialiseFailure IO LBS.ByteString
codecHaskell =
    mkCodecCborLazyBS' enc dec
  where
    enc :: forall (pr :: PeerRole) st st'.
              PeerHasAgency pr st
           -> Message (Haskell rej) st st'
           -> CBOR.Encoding
    enc (ClientAgency TokIdle) (MsgRequest r) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 0
     <> encode r

    enc (ServerAgency TokBusy) (MsgReply r) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 1
     <> encode r

    enc (ServerAgency TokBusy) (MsgBadRequest t) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 2
     <> encode t

    enc (ClientAgency TokIdle) MsgDone =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 3


    dec :: forall (pr :: PeerRole) s (st :: (Haskell rej)).
              PeerHasAgency pr st
           -> CBOR.Decoder s (SomeMessage st)
    dec stok = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case (stok, len, key) of
        (ClientAgency TokIdle, 2, 0) -> do
          r <- decode
          return (SomeMessage (MsgRequest r))

        (ServerAgency TokBusy, 2, 1) -> do
          r <- decode
          return (SomeMessage (MsgReply r))

        (ServerAgency TokBusy, 2, 2) -> do
          reject <- decode
          return (SomeMessage (MsgBadRequest reject))

        (ClientAgency TokIdle, 1, 3) ->
          return (SomeMessage MsgDone)

        (ClientAgency TokIdle, _, _) ->
          fail "codecHaskellMessages.Idle: unexpected key"
        (ServerAgency TokBusy, _, _) ->
          fail "codecHaskellMessages.Busy: unexpected key"

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

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

deriving instance Generic (G.Graph a)
instance Serialise a => Serialise (G.Graph a) where
