{-# OPTIONS_GHC -Wextra -Wno-unused-binds -Wno-missing-fields -Wno-all-missed-specialisations -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Lift
  ( main
  , defaultConfig
  , Config(..)
  , liftCiphers
  )
where

import Codec.Serialise
import Data.ByteString                  qualified as BS
import Data.ByteString.Lazy             qualified as LBS
import Data.Default                                 (def)
import Data.IntUnique
import Data.List                        qualified as List
import Data.Map                         qualified as Map
import Data.Map                         qualified as Map
import Data.Map                                     (Map)
import Data.Set.Monad                   qualified as Set
import Data.Set.Monad                               (Set)
import Data.Text
import Data.X509.Validation             qualified as X509
import Data.X509.CertificateStore       qualified as X509
import GHC.Generics                                 (Generic)
import GHC.Types                                    (Symbol)
import GHC.TypeLits
import GHC.TypeLits                     qualified as Ty
import Options.Applicative
import Options.Applicative.Common
import Type.Reflection                  qualified as R

import Control.Concurrent               qualified as Conc
import Control.Concurrent.Async         qualified as Conc
import Control.Concurrent.Chan.Unagi                (InChan, OutChan,
                                                     newChan, readChan, writeChan)
import Control.Concurrent.STM           qualified as STM
import Control.Concurrent.STM                       (STM, TVar, atomically)
import Control.Exception                     hiding (TypeError)
import Control.Monad                                (forever)
import Control.Tracer
import Data.Time
import Network.WebSockets               qualified as WS
import Network.WebSockets.Connection    qualified as WS
import Network.WebSockets.Connection.Options qualified as WS
import Network.WebSockets.Stream        qualified as WS
import Shelly                                       (liftIO, run, shelly)
import System.Environment
import Unsafe.Coerce                    qualified as Unsafe

import Network.TypedProtocol.Channel    qualified as Net

import Reflex                                hiding (Request)
import Reflex.Network


import Generics.SOP.Some
import Generics.SOP.Some                qualified as SOP

import Basis

import Dom.CTag
import Dom.Cap
import Dom.Error
import Dom.Expr
import Dom.Ground.Hask
import Dom.Ground.Hask qualified as Hask
import Dom.Located
import Dom.LTag
import Dom.Name
import Dom.Pipe
import Dom.Pipe.EPipe
import Dom.Pipe.Ops
import Dom.Pipe.SomePipe
import Dom.RequestReply
import Dom.Result
import Dom.Sig
import Dom.SomeValue
import Dom.Space.SomePipe
import Dom.Value

import Dom.Cred
import Dom.Space.Pipe

import Ground.Expr
import Ground.Table

import Wire.MiniProtocols
import Wire.Peer
import Wire.Protocol
import Wire.WSS

import Lift.Pipe
import Lift.Server

import TopHandler

import Reflex.Lift.Host

import Network.TLS qualified as TLS
import Network.TLS.Extra.Cipher qualified as TLS

import Debug.Reflex


-- * Actually do things.
--
deriveConfig :: Config Initial -> IO Env
deriveConfig preConfig@Config{..} = do
  libDir <- case cfGhcLibDir of
    Just x  -> pure x
    Nothing -> (FileName <$>) <$> shelly $ run "ghc" ["--print-libdir"]

  let envTracer = stdoutTracer
      envConfig = preConfig { cfGhcLibDir  = libDir }

  pure Env{..}

main :: IO ()
main = toplevelExceptionHandler $ do
  commd <- execParser opts

  env@Env{envConfig=Config{..}} <- deriveConfig defaultConfig

  sealGround

  case commd of
    Daemon -> do
      let creds  = CredFile "server-certificate.pem" "server-key.pem"
          addr   = WSAddr "127.0.0.1" cfWSPortOut "/"
          showTr = showTracing (contramap pack stderr)
          trs    =
            WireTracers
            { trMux         = showTr
            , trMuxRequests = showTr
            , trMuxReplies  = showTr
            , trWss         = showTr
            , trBearer      = nullTracer
            }
      runWssServer trs creds addr (startServer env)
    Exec rq ->
      handleRequest env (unsafeCoerceUnique 0, rq)
      >>= either print (mapSomeResult print)

 where
   startServer ::
        Env
     -> IO (RequestServer EPipe IO (),
            RepliesServer EPipe IO StandardReply)
   startServer env = do
     (reqsW, reqsR) :: (InChan   StandardRequest,
                        OutChan  StandardRequest) <- newChan
     (repsW, repsR) :: (InChan   StandardReply,
                        OutChan  StandardReply)   <- newChan

     void $ Conc.async $
       liftRequestLoop env reqsR repsW

     pure $
       (,)
       (ServingRequest (writeChan reqsW))
       (ServingReply   (readChan  repsR))

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
  | Exec (Request (Located (QName Pipe)))
