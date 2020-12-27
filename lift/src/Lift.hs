{-# OPTIONS_GHC -Wextra -Wno-unused-binds -Wno-missing-fields -Wno-all-missed-specialisations -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Lift
  ( main
  , defaultConfig
  , Config(..)
  , channelFromWebsocket
  , liftCiphers
  )
where

import Codec.Serialise
import Data.ByteString qualified                  as BS
import Data.ByteString.Lazy qualified             as LBS
import Data.Default                                 (def)
import Data.List                        qualified as List
import Data.Map qualified                         as Map
import Data.Map qualified                         as Map
import Data.Map                                     (Map)
import Data.Set.Monad qualified                   as Set
import Data.Set.Monad                               (Set)
import Data.Text
import Data.X509.Validation             qualified as X509
import Data.X509.CertificateStore       qualified as X509
import GHC.Generics                                 (Generic)
import GHC.Types                                    (Symbol)
import GHC.TypeLits
import GHC.TypeLits qualified                     as Ty
import Options.Applicative
import Options.Applicative.Common
import Type.Reflection qualified                  as R

import Control.Concurrent qualified               as Conc
import Control.Concurrent.STM qualified           as STM
import Control.Concurrent.STM                       (STM, TVar, atomically)
import Control.Exception                     hiding (TypeError)
import Control.Monad                                (forever)
import Control.Tracer
import Data.Time
import Network.WebSockets qualified               as WS
import Network.WebSockets.Connection    qualified as WS
import Network.WebSockets.Connection.Options qualified as WS
import Network.WebSockets.Stream        qualified as WS
import Shelly                                       (liftIO, run, shelly)
import System.Environment
import Unsafe.Coerce qualified                    as Unsafe

import Network.TypedProtocol.Channel qualified    as Net

import Generics.SOP.Some
import Generics.SOP.Some qualified                as SOP

import Basis

import Dom.CTag
import Dom.Cap
import Dom.Error
import Dom.Expr
import Dom.Ground.Hask
import Dom.Ground.Hask qualified as Hask
import Dom.Located
import Dom.Name
import Dom.Pipe
import Dom.Pipe.EPipe
import Dom.Pipe.Ops
import Dom.Pipe.SomePipe
import Dom.RequestReply
import Dom.Sig
import Dom.SomeValue
import Dom.Space.SomePipe
import Dom.Value

import Ground.Expr
import Ground.Table

import Wire.Peer
import Wire.Protocol

import Lift.Pipe

import TopHandler


import Dom.Space.Pipe

import Network.TLS qualified as TLS
import Network.TLS.Extra.Cipher qualified as TLS


-- * Actually do things.
--
main :: IO ()
main = toplevelExceptionHandler $ do
  sealGround
  commd <- execParser opts
  let preConfig@Config{..} = defaultConfig

  libDir <- case cfGhcLibDir of
    Just x  -> pure x
    Nothing -> (FileName <$>) <$> shelly $ run "ghc" ["--print-libdir"]

  let config :: Config Final = preConfig
        { cfGhcLibDir  = libDir }

  config' <- finalise config

  case commd of
    Daemon -> wsServer config'
    Exec rq -> handleRequest config' rq >>= print

 where
   opts = info (cli <**> helper)
     ( fullDesc
    <> progDesc "A simple lift"
    <> header "lift - execute requests, either on CLI or over WebSockets" )
   cli :: Parser Cmd
   cli = subparser $ mconcat
     [ cmd "daemon" $ pure Daemon
     , cmd "exec" $ Exec <$> cliRequest
     ]
   cmd name p = command name $ info (p <**> helper) mempty

data Cmd
  = Daemon
  | Exec StandardRequest


data ConfigPhase
  = Initial
  | Final

data Config a =
  Config
  { cfWSBindHost :: String
  , cfWSPortIn   :: Int
  , cfWSPortOut  :: Int
  , cfWSPingTime :: Int
  , cfGhcLibDir  :: CFGhcLibDir a
  , cfGitRoot    :: FileName
  , cfHackageTmo :: NominalDiffTime
  }
type family CFGhcLibDir (a :: ConfigPhase) where
  CFGhcLibDir Initial  = Maybe FileName
  CFGhcLibDir Final    = FileName

defaultConfig :: Config Initial
defaultConfig = Config
  { cfWSBindHost = "127.0.0.1"
  , cfWSPortIn   = 29670
  , cfWSPortOut  = 29671
  , cfWSPingTime = 30
  , cfGhcLibDir  = Nothing
  , cfGitRoot    = "."
  , cfHackageTmo = 3600
  }

data Env =
  Env
  { envConfig      :: !(Config Final)
  , envTracer      :: !(Tracer IO String)
  }

finalise :: Config Final -> IO Env
finalise envConfig@Config{} = do
  let envTracer = stdoutTracer
  pure Env{..}

channelFromWebsocket :: WS.Connection -> Net.Channel IO LBS.ByteString
channelFromWebsocket conn =
  Net.Channel
  { recv = catch (Just <$> WS.receiveData conn)
           (\(SomeException _x) -> pure Nothing)
  , send = WS.sendBinaryData conn
  }
  where
    tracer :: Show x => String -> x -> x
    tracer desc x = trace (printf "%s:\n%s\n" desc (show x)) x

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

wsServer :: HasCallStack => Env -> IO ()
wsServer env@Env{envConfig=Config{..},..} = forever $ do
  Just srvCA <- liftIO $ X509.readCertificateStore "server-certificate.pem"
  traceM $ "Started server on " <> show cfWSBindHost <> ":" <> show cfWSPortOut
  flip catches
   [ Handler $
     \(SomeException (e :: a)) -> do
       traceM $ Prelude.concat
         [ "Uncaught WS.runServerWithOptions exception ("
         , unpack $ showTypeRepNoKind $ typeRep @a, "): "
         , show e
         ]
       pure ()] $
   WS.runServerWithOptions
    (WS.defaultServerOptions
     { WS.serverHost              = cfWSBindHost
     , WS.serverPort              = cfWSPortOut
     , WS.serverConnectionOptions =
       WS.defaultConnectionOptions
       { WS.connectionTlsSettings =
         Just WS.defaultTlsSettings
         { WS.tlsAllowedVersions  = [TLS.TLS13]
         , WS.certSettings        = WS.CertFromFile
                                      "server-certificate.pem"
                                      []
                                      "server-key.pem"
         , WS.tlsLogging          = def
           -- { TLS.loggingPacketRecv = traceM, TLS.loggingPacketSent = traceM }
         , WS.tlsWantClientCert   = True
         , WS.tlsServerHooks      =
           def
           { TLS.onClientCertificate =
             \certChain ->
               X509.validateDefault srvCA def ("127.0.0.1", mempty)
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
      traceM $ mconcat
        [ "Accepted connection on ", show cfWSBindHost, ":", show cfWSPortOut, ", "
        , show $ TLS.infoVersion tlsCtxInfo, ", "
        , "cipher ", show $ TLS.infoCipher tlsCtxInfo, ", "
        , "compression ", show $ TLS.infoCompression tlsCtxInfo
        ]
      conn <- WS.acceptRequest pending
      WS.withPingThread conn cfWSPingTime (pure ()) $ do
        let handleDisconnect :: WS.ConnectionException -> IO ()
            handleDisconnect x = putStrLn $ "Web disconnected: " <> show x
            tracer = stdoutTracer

        void $ catches
          (runServer tracer (haskellServer env) $ channelFromWebsocket conn)
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

haskellServer
  :: forall m a
  . (m ~ IO, a ~ Reply)
  => Env
  -> Server EPipe m a
haskellServer env@Env{..} =
    server
  where
    server = Server {
      processRequest = \req -> do
        traceWith (showTracing envTracer) req
        r <- handleRequest env req
        pure (r, server)
    , processDone = ()
    }

handleRequest :: Env -> StandardRequest -> IO (Either EPipe Reply)
handleRequest Env{} req = runExceptT $ case req of
  Run      expr -> runRun      expr
  Let name expr -> runLet name expr

runRun :: HasCallStack => Expr (Located (QName Pipe)) -> ExceptT EPipe IO Reply
runRun expr = do
  spc <- liftIO $ atomically getState

  runnable <- newExceptT . pure $
    compile opsFull (lookupSomePipe spc) expr

  result <- ReplyValue <$> newPipeExceptErr EExec (runSomePipe runnable)

  pure result

runLet :: QName Pipe -> Expr (Located (QName Pipe)) -> ExceptT EPipe IO Reply
runLet name expr = do
    liftIO $ putStrLn $ unpack $ mconcat
      ["let ", showQName name, " = ", pack $ show (locVal <$> expr) ]

    spc <- liftIO $ atomically getState

    void . newPipeExceptErr EName . pure $
      maybeLeft (lookupSomePipe spc
                 >>> ($> Error ("Already exists: " <> showQName name)))
                name

    pipe <- newExceptT . pure $
      compile opsFull (lookupSomePipe spc) expr

    void . newExceptT . fmap (left (EName . Error)) . liftIO . atomically $
      addPipe name pipe

    liftIO $
      putStrLn =<< unpack . showPipeSpace <$> atomically (STM.readTVar mutablePipeSpace)

    liftIO $ ReplyValue
      . SV CPoint . SVK VPipeSpace capsTSG
      . mkValue CPoint VPipeSpace <$>
      getThePipeSpace
