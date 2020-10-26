{-# OPTIONS_GHC -Wextra -Wno-unused-binds -Wno-missing-fields -Wno-all-missed-specialisations -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Lift
  ( main
  , defaultConfig
  , Config(..)
  , channelFromWebsocket
  )
where

import           Codec.Serialise
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.Map                         as Map
import           Data.Map                           (Map)
import qualified Data.Set.Monad                   as Set
import           Data.Set.Monad                     (Set)
import           Data.Text
import           GHC.Generics                       (Generic)
import           GHC.Types                          (Symbol)
import           GHC.TypeLits
import qualified GHC.TypeLits                     as Ty
import           Options.Applicative
import           Options.Applicative.Common
import qualified Type.Reflection                  as R

-- import qualified Control.Concurrent.Chan.Unagi    as Unagi
import qualified Control.Concurrent               as Conc
import qualified Control.Concurrent.STM           as STM
import           Control.Concurrent.STM             (STM, TVar, atomically)
import           Control.Exception           hiding (TypeError)
import           Control.Monad                      (forever)
import           Control.Tracer
import           Data.Time
import qualified Network.WebSockets               as WS
import           Shelly                             (run, shelly)
import           System.Environment
import qualified Unsafe.Coerce                    as Unsafe

-- import qualified Cardano.BM.Configuration         as BM
-- import qualified Cardano.BM.Configuration.Model   as BM
-- import qualified Cardano.BM.Setup                 as BM
-- import qualified Cardano.BM.Backend.Switchboard   as BM
-- import qualified Cardano.BM.Trace                 as BM

import qualified Network.TypedProtocol.Channel    as Net

import           Generics.SOP.Some
import qualified Generics.SOP.Some                as SOP

import Basis

import Dom.CTag
import Dom.Error
import Dom.Expr
import Dom.Ground.Hask
import qualified Dom.Ground.Hask as Hask
import Dom.Located
import Dom.Name
import Dom.Pipe
import Dom.Pipe.Ops
import Dom.Pipe.SomePipe
import Dom.Sig
import Dom.SomeValue
import Dom.Space.SomePipe
import Dom.Value

import Ground.Expr
import Ground.Table

import Lift.Pipe

import Wire.Peer
import Wire.Protocol



-- * Actually do things.
--
main :: IO ()
main = do
  sealGround
  cmd <- execParser opts
  let preConfig@Config{..} = defaultConfig

  libDir <- case cfGhcLibDir of
    Just x  -> pure x
    Nothing -> (FileName <$>) <$> shelly $ run "ghc" ["--print-libdir"]

  let config :: Config Final = preConfig
        { cfGhcLibDir  = libDir }

  config' <- finalise config

  case cmd of
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
     , cmd "exec" $ Exec <$> parseRequest
     ]
   cmd name p = command name $ info (p <**> helper) mempty

data Cmd
  = Daemon
  | Exec Request


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
finalise envConfig@Config{..} = do
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

wsServer :: Env -> IO ()
wsServer env@Env{envConfig=Config{..},..} = forever $
  WS.runServer cfWSBindHost cfWSPortOut $
    \pending-> do
      conn <- WS.acceptRequest pending
      WS.forkPingThread conn cfWSPingTime

      let handleDisconnect :: WS.ConnectionException -> IO ()
          handleDisconnect x = putStrLn $ "Web disconnected: " <> show x
          tracer = stdoutTracer

      handle handleDisconnect . forever $
        runServer tracer (haskellServer env) $ channelFromWebsocket conn

haskellServer
  :: forall m a
  . (m ~ IO, a ~ Reply)
  => Env
  -> Server Error m a
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

handleRequest :: Env -> Request -> IO (Fallible Reply)
handleRequest Env{..} x = case x of
  Compile name pipe ->
    Lift.compile name pipe
    <&> (ReplyValue . SomeValue TPoint . SomeValueKinded VSig . VPoint <$>)
  Run nameTree -> do
    putStrLn $ unpack $ Data.Text.unlines
      ["Pipe:", pack $ show (locVal <$> nameTree) ]
    spc :: SomePipeSpace Dynamic <- atomically getState
    case Dom.Pipe.Ops.compile opsFull (lookupSomePipe spc) nameTree of
      Left (Error e) -> pure $ fallDesc "Compilation" e
      Right (runnable :: SomePipe Dynamic) -> do
        putStrLn "ok, runnable"
        res :: Fallible SomeValue <- runSomePipe runnable
        putStrLn "ok, ran (?)"
        case res of
          Left (Error e) -> pure $ fallDesc "Runtime" e
          Right x -> pure . Right . ReplyValue $ x

compile :: QName Pipe -> Expr (Located (QName Pipe)) -> Result ISig
compile newname e = do
      putStrLn $ unpack $ Data.Text.unlines
        ["Pipe:", pack $ show (locVal <$> e) ]
      atomically $ do
        spc <- getState
        let old = lookupSomePipe spc newname
            res = Dom.Pipe.Ops.compile opsFull (lookupSomePipe spc) e
        case (old, res) of
          (Just _,  _)    -> fallM $ "Already exists: " <> pack (show newname)
          (_,  Left e)    -> pure $ Left e
          (_, Right pipe) -> do
            addPipe newname pipe
            pure . Right . somePipeSig $ pipe
