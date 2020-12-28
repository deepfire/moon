{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Wire.WSS (module Wire.WSS) where

import Control.Exception                          hiding (TypeError)
import Control.Tracer

import Data.ByteString.Lazy                  qualified as LBS
import Data.Default                                      (def)
import Data.List                             qualified as List

import Data.X509.Validation                  qualified as X509

import Network.Mux                           qualified as Mux
import Network.Mux.Compat                    qualified as Mux
import Ouroboros.Network.Mux                 qualified as ONet

import Network.WebSockets                    qualified as WS
import Network.WebSockets.Connection         qualified as WS
import Network.WebSockets.Connection.Options qualified as WS
import Network.WebSockets.Stream             qualified as WS

import Network.Connection                    qualified as Net
import Network.TLS                           qualified as TLS
import Network.TLS.Extra.Cipher              qualified as TLS
import Network.TLS.SessionManager            qualified as SM

import Basis

import Dom.Cred
import Dom.Pipe.EPipe
import Dom.RequestReply

import Wire.MiniProtocols
import Wire.Peer
import Wire.WSS.Bearer


data WSAddr
  = WSAddr
  { wsaHost :: !String
  , wsaPort :: !Int
  , wsaPath :: !String
  }

runWssClient ::
  forall m rej a
  . ( m   ~ IO
    , rej ~ EPipe
    , a   ~ StandardReply)
  => WireTracers m
  -> CredSpec
  -> WSAddr
  -> m (RequestClient rej m (), RepliesClient rej m ())
  -> m ()
runWssClient trs cspec WSAddr{..} mkClients = do
  Cred{cIdentity, cCred=TLS.Credentials (cred:_)} <-
    loadCred cspec
      <&> flip either id (error . ("Failed to load credentials: " <>))
  sm <- SM.newSessionManager SM.defaultConfig
  let tlsSettings =
        Net.TLSSettings
        (TLS.defaultParamsClient "127.0.0.1" "")
        { TLS.clientUseServerNameIndication = False
        , TLS.clientSupported =
          def
          { TLS.supportedCiphers  = liftCiphers
          , TLS.supportedVersions = [TLS.TLS13]
          }
        , TLS.clientShared =
          def
          { TLS.sharedSessionManager = sm
          , TLS.sharedCAStore = cIdentity
          -- , TLS.sharedCredentials = TLS.Credentials [cred]
          }
        , TLS.clientHooks =
          def
          { TLS.onCertificateRequest =
            \(_certTys, _mHashes, _dName) ->
              pure $ Just cred
          }
        }
      connectionParams = Net.ConnectionParams
        { connectionHostname  = wsaHost
        , connectionPort      = toEnum wsaPort
        , connectionUseSecure = Just tlsSettings
        , connectionUseSocks  = Nothing
        }
  context <- Net.initConnectionContext
  connection <- Net.connectTo context connectionParams
  stream <- WS.makeStream
    (fmap Just (Net.connectionGetChunk connection))
    (maybe (return ()) (Net.connectionPut connection . LBS.toStrict))

  (,) requestInitiator repliesInitiator
     <- mkClients <&>
        ((mkPeerInitiator (trMuxRequests trs) wireCodecRequests
          . mkRequestClientSTS . pure)
         ***
         (mkPeerInitiator (trMuxReplies  trs) wireCodecReplies
          . mkRepliesClientSTS . pure))
  let headers = []
  WS.runClientWithStream stream wsaHost wsaPath WS.defaultConnectionOptions headers
    \conn ->
      Mux.muxStart
        (trMux trs)
        (Mux.MuxApplication
          [ Mux.MuxMiniProtocol
            { Mux.miniProtocolNum    = Mux.MiniProtocolNum 0
            , Mux.miniProtocolLimits =
              Mux.MiniProtocolLimits
              { maximumIngressQueue = 3000000
              }
            , Mux.miniProtocolRun    = toMuxRunMiniProtocol requestInitiator
                  :: Mux.RunMiniProtocol 'ONet.InitiatorMode IO () Void
            }
          , Mux.MuxMiniProtocol
            { Mux.miniProtocolNum    = Mux.MiniProtocolNum 1
            , Mux.miniProtocolLimits =
              Mux.MiniProtocolLimits
              { maximumIngressQueue = 3000000
              }
            , Mux.miniProtocolRun    = toMuxRunMiniProtocol repliesInitiator
                  :: Mux.RunMiniProtocol 'ONet.InitiatorMode IO () Void
            }
          ])
        (wsConnectionAsMuxBearer (trBearer trs) conn)

runWssServer ::
     WireTracers IO
  -> CredSpec
  -> WSAddr
  -> IO (RequestServer EPipe IO (), RepliesServer EPipe IO StandardReply)
  -> IO ()
runWssServer trs cspec WSAddr{..} mkServers = do
  Cred{cIdentity} <- loadCred cspec <&> flip either id
    (error . ("Failed to load credentials: " <>))

  traceWith (trWss trs) $ "Starting server on " <> showT wsaHost <> ":" <> showT wsaPort
  flip catches
   [ Handler $
     \(SomeException (e :: a)) -> do
       traceWith (trWss trs) $ mconcat
         [ "Uncaught WS.runServerWithOptions exception ("
         , showT $ typeRep @a, "): "
         , showT e
         ]
       pure ()] $
   WS.runServerWithOptions
    (WS.defaultServerOptions
     { WS.serverHost              = wsaHost
     , WS.serverPort              = wsaPort
     , WS.serverConnectionOptions =
       WS.defaultConnectionOptions
       { WS.connectionTlsSettings =
         Just WS.defaultTlsSettings
         { WS.tlsAllowedVersions  = [TLS.TLS13]
         , WS.certSettings        = toWSCertSettings cspec
         , WS.tlsLogging          = def
           -- { TLS.loggingPacketRecv = traceM, TLS.loggingPacketSent = traceM }
         , WS.tlsWantClientCert   = True
         , WS.tlsServerHooks      =
           def
           { TLS.onClientCertificate =
             \certChain ->
               X509.validateDefault cIdentity def ("127.0.0.1", mempty)
                 certChain <&>
                 \case
                   [] -> TLS.CertificateUsageAccept
                   errs -> TLS.CertificateUsageReject .
                           TLS.CertificateRejectOther $
                           List.intercalate ", " (show <$> errs)
           }
         }
       }
     }) $
    \pending -> do
      let Just tlsCtx = WS.streamTlsContext $ WS.pendingStream pending
      Just tlsCtxInfo <- TLS.contextGetInformation tlsCtx
      traceWith (trWss trs) $ mconcat
        [ "Accepted connection on ", showT wsaHost, ":", showT wsaPort, ", "
        , showT $ TLS.infoVersion tlsCtxInfo, ", "
        , "cipher ", showT $ TLS.infoCipher tlsCtxInfo, ", "
        , "compression ", showT $ TLS.infoCompression tlsCtxInfo
        ]
      conn <- WS.acceptRequest pending
      WS.withPingThread conn 60 (pure ()) $ do
        (,) requestResponder repliesResponder
           <- mkServers <&>
              ((mkPeerResponder (trMuxRequests trs) wireCodecRequests
                . mkRequestServerSTS . pure)
               ***
               (mkPeerResponder (trMuxReplies  trs) wireCodecReplies
                . mkRepliesServerSTS . pure))

        void $ catches
          (Mux.muxStart
            (trMux trs)
            (Mux.MuxApplication
              [ Mux.MuxMiniProtocol
                { Mux.miniProtocolNum    = Mux.MiniProtocolNum 0
                , Mux.miniProtocolLimits =
                  Mux.MiniProtocolLimits
                  { maximumIngressQueue = 3000000
                  }
                , Mux.miniProtocolRun    = toMuxRunMiniProtocol requestResponder
                }
              , Mux.MuxMiniProtocol
                { Mux.miniProtocolNum    = Mux.MiniProtocolNum 1
                , Mux.miniProtocolLimits =
                  Mux.MiniProtocolLimits
                  { maximumIngressQueue = 3000000
                  }
                , Mux.miniProtocolRun    = toMuxRunMiniProtocol repliesResponder
                }
              ])
            (wsConnectionAsMuxBearer (trBearer trs) conn))
          [ Handler $
            \(SomeException (e :: a)) -> do
              traceWith (trWss trs) $ mconcat
                [ "Uncaught Wire.Peer.runServer exception ("
                , showTypeRepNoKind $ typeRep @a, "): ", showT e]
          , Handler $
            \(e :: WS.ConnectionException) -> do
              traceWith (trWss trs) $ mconcat
                [ "Disconnected: ", showT e ]
          ]

-- | The strongest ciphers supported.  For ciphers with PFS, AEAD and SHA2, we
-- list each AES128 variant after the corresponding AES256 and ChaCha20-Poly1305
-- variants.  For weaker constructs, we use just the AES256 form.
--
-- The CCM ciphers come just after the corresponding GCM ciphers despite their
-- relative performance cost.
liftCiphers :: [TLS.Cipher]
liftCiphers =
  -- PFS + AEAD + SHA2 only
  [ TLS.cipher_ECDHE_ECDSA_CHACHA20POLY1305_SHA256
  , TLS.cipher_ECDHE_RSA_CHACHA20POLY1305_SHA256
  , TLS.cipher_ECDHE_ECDSA_AES256GCM_SHA384,
    TLS.cipher_ECDHE_ECDSA_AES256CCM_SHA256
  , TLS.cipher_ECDHE_RSA_AES256GCM_SHA384
  , TLS.cipher_DHE_RSA_CHACHA20POLY1305_SHA256
  , TLS.cipher_DHE_RSA_AES256GCM_SHA384
  , TLS.cipher_DHE_RSA_AES256CCM_SHA256
  -- TLS13 (listed at the end but version is negotiated first)
  , TLS.cipher_TLS13_CHACHA20POLY1305_SHA256
  , TLS.cipher_TLS13_AES256GCM_SHA384
  , TLS.cipher_TLS13_AES128GCM_SHA256
  , TLS.cipher_TLS13_AES128CCM_SHA256
  ]
