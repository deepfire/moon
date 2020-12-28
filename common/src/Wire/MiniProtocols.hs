{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Wire.MiniProtocols (module Wire.MiniProtocols) where

import Codec.Serialise                       qualified as CBOR
import Control.Monad.Class.MonadAsync                    (MonadAsync)
import Control.Monad.Class.MonadThrow                    (MonadCatch)
import Control.Tracer

import Data.ByteString.Lazy                  qualified as LBS
import Data.Text
import Data.Void

import Ouroboros.Network.Channel             qualified as ONet
import Ouroboros.Network.Driver              qualified as ONet
import Ouroboros.Network.Mux                 qualified as ONet
import Ouroboros.Network.Util.ShowProxy      qualified as ONet

import Network.Mux.Compat                    qualified as Mux

import Network.TypedProtocol.Core            qualified as Proto
import Network.TypedProtocol.Codec           qualified as Proto

import Dom.Pipe.EPipe

import Wire.Protocol


data WireTracers m
  = WireTracers
    { trMux         :: !(Tracer m Mux.MuxTrace)
    , trMuxRequests :: !(Tracer m (ONet.TraceSendRecv (Requests EPipe)))
    , trMuxReplies  :: !(Tracer m (ONet.TraceSendRecv (Replies EPipe)))
    , trBearer      :: !(Tracer m Mux.MuxTrace)
    , trWss         :: !(Tracer m Text)
    }

mkPeerInitiator ::
  ( ONet.ShowProxy proto
  , forall st'. Show (ServerHasAgency @proto st')
  , forall st'. Show (ClientHasAgency @proto st'))
  => Tracer IO (ONet.TraceSendRecv proto)
  -> Proto.Codec proto CBOR.DeserialiseFailure IO bytes
  -> Proto.Peer proto Proto.AsClient (st :: proto) IO a
  -> ONet.RunMiniProtocol Mux.InitiatorMode bytes IO a Void
mkPeerInitiator tr codec peer =
  ONet.InitiatorProtocolOnly $
    ONet.MuxPeer tr codec peer

mkPeerResponder ::
  ( ONet.ShowProxy proto
  , forall st'. Show (ServerHasAgency @proto st')
  , forall st'. Show (ClientHasAgency @proto st'))
  => Tracer IO (ONet.TraceSendRecv proto)
  -> Proto.Codec proto CBOR.DeserialiseFailure IO bytes
  -> Proto.Peer proto Proto.AsServer (st :: proto) IO b
  -> ONet.RunMiniProtocol Mux.ResponderMode bytes IO Void b
mkPeerResponder tr codec peer =
  ONet.ResponderProtocolOnly $
    ONet.MuxPeer tr codec peer

toMuxRunMiniProtocol :: forall mode m a b.
                        (MonadCatch m, MonadAsync m)
                     => ONet.RunMiniProtocol mode LBS.ByteString m a b
                     -> Mux.RunMiniProtocol mode m a b
toMuxRunMiniProtocol (ONet.InitiatorProtocolOnly i) =
  Mux.InitiatorProtocolOnly (ONet.runMuxPeer i . ONet.fromChannel)
toMuxRunMiniProtocol (ONet.ResponderProtocolOnly r) =
  Mux.ResponderProtocolOnly (ONet.runMuxPeer r . ONet.fromChannel)
toMuxRunMiniProtocol (ONet.InitiatorAndResponderProtocol i r) =
  Mux.InitiatorAndResponderProtocol (ONet.runMuxPeer i . ONet.fromChannel)
                                    (ONet.runMuxPeer r . ONet.fromChannel)
