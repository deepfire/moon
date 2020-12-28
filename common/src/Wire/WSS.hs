module Wire.WSS (module Wire.WSS) where

import Control.Exception                          hiding (TypeError)
import Control.Tracer

import Data.ByteString.Lazy                  qualified as LBS
import Data.Default                                      (def)
import Data.List                             qualified as List
import Data.Text

import Data.X509.Validation                  qualified as X509

import Network.TypedProtocol.Channel         qualified as Net

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

import qualified Wire.Peer                     as Wire


data WSAddr
  = WSAddr
  { wsaHost :: String
  , wsaPort :: Int
  , wsaPath :: String
  }

channelFromWebsocket :: WS.Connection -> Net.Channel IO LBS.ByteString
channelFromWebsocket conn =
  Net.Channel
  { recv = catch (Just <$> WS.receiveData conn)
           (\(SomeException _x) -> pure Nothing)
  , send = WS.sendBinaryData conn
  }
  where
    _tracer :: Show x => String -> x -> x
    _tracer desc x = trace (printf "%s:\n%s\n" desc (show x)) x

runWssClient ::
     Tracer IO Text
  -> CredSpec
  -> WSAddr
  -> IO (Wire.ClientState EPipe IO Reply)
  -> IO ()
runWssClient tr cspec WSAddr{..} client = do
  Cred{cIdentity, cCred=TLS.Credentials (cred:_)} <-
    loadCred cspec
      <&> flip either id (error . ("Failed to load credentials: " <>))
  sm <- SM.newSessionManager SM.defaultConfig
  let tlsSettings =
        Net.TLSSettings
      -- This is the important setting.
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
          -- Q: what's this for?
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
  let headers = []
  WS.runClientWithStream stream wsaHost wsaPath WS.defaultConnectionOptions headers
  -- WS.runSecureClient wsaHost (toEnum wsaPort) wsaPath
    \(channelFromWebsocket -> webSockChan) -> do
      void $ Wire.runClient tr webSockChan client

runWssServer ::
     Tracer IO Text
  -> CredSpec
  -> WSAddr
  -> Wire.Server EPipe IO Reply
  -> IO ()
runWssServer tr cspec WSAddr{..} server = do
  Cred{cIdentity} <- loadCred cspec <&> flip either id
    (error . ("Failed to load credentials: " <>))

  traceWith tr $ "Starting server on " <> showT wsaHost <> ":" <> showT wsaPort
  flip catches
   [ Handler $
     \(SomeException (e :: a)) -> do
       traceWith tr $ mconcat
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
      traceWith tr $ mconcat
        [ "Accepted connection on ", showT wsaHost, ":", showT wsaPort, ", "
        , showT $ TLS.infoVersion tlsCtxInfo, ", "
        , "cipher ", showT $ TLS.infoCipher tlsCtxInfo, ", "
        , "compression ", showT $ TLS.infoCompression tlsCtxInfo
        ]
      conn <- WS.acceptRequest pending
      WS.withPingThread conn 60 (pure ()) $ do
        let handleDisconnect :: WS.ConnectionException -> IO ()
            handleDisconnect x = putStrLn $ "Web disconnected: " <> show x

        void $ catches
          (Wire.runServer tr server $ channelFromWebsocket conn)
          [ Handler $
            \(SomeException (e :: a)) -> do
              traceM $ Prelude.concat
                [ "Uncaught Wire.Peer.runServer exception ("
                , unpack $ showTypeRepNoKind $ typeRep @a, "): "
                , show e
                ]
              pure ()
          , Handler $
            \(e :: WS.ConnectionException) -> do
              traceM $ Prelude.concat
                [ "Disconnected: "
                , show e
                ]
              handleDisconnect e
              pure ()
          ]
        pure ()

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
