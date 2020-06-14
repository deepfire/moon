{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs   #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
--{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import           Safe

import           Codec.Serialise                          (Serialise)
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.NodeId
import           Control.Tracer
                 (Tracer, stdoutTracer, showTracing, traceWith)
import qualified Control.Zipper                         as Zip
import           Data.Functor.Misc
import qualified Data.IntervalMap.FingerTree            as IMap
import qualified Data.List                              as List
import           Data.Map (Map)
import           Data.Maybe
import           Data.Maybe
import           Data.Text
import           Data.Text (Text)
import           Data.Tuple.All
import           Options.Applicative               hiding (switch)
import           Options.Applicative.Common
import           Reflex                            hiding (Request)
import           Reflex.Class.Switchable
import           Reflex.Network
import           Reflex.Vty                        hiding (Request)
import           Reflex.Vty.Widget.Layout
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.Char                              as C
import qualified Data.Map                               as Map
import qualified Data.Text                              as T
import qualified Data.Text.Zipper                       as TZ
import qualified Graphics.Vty                           as V
import qualified Network.WebSockets                     as WS
-- import qualified Control.Concurrent.STM.TMVar           as TM
-- import qualified Control.Concurrent.STM.TBQueue         as TQ

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Core
import qualified Network.TypedProtocol.Core             as Net
import qualified Network.TypedProtocol.Driver           as Net

import           Reflex.Vty.Widget.Extra
import qualified Reflex.Vty.Widget.Input.RichText       as Rich

import Basis hiding (Dynamic)
import Data.Text.Extra
import qualified Data.Text.Zipper.Extra as TZ
import Ground.Hask
import Lift
import Pipe hiding (Dynamic)
import Wire.Peer
import Wire.Protocol
import Debug.Reflex

import Debug.TraceErr
import Text.Printf


main :: IO ()
main = do
  mreq <- execParser $ (info $ (optional parseRequest) <**> helper) fullDesc
  WS.runClient "127.0.0.1" (cfWSPortOut defaultConfig) "/" $
    \conn -> do
      let tracer = stdoutTracer
          req    = fromMaybe (Run "meta.space") mreq
          chan   = channelFromWebsocket conn
      runClient tracer (client tracer req) $ channelFromWebsocket conn

client :: forall rej m a
          . (rej ~ Text, m ~ IO, a ~ Reply)
       => Tracer m String
       -> Request
       -> m (ClientState rej m a)
client tracer req = pure $
  ClientRequesting req handleReply
 where
   handleReply (Left rej) = do
     putStrLn $ "error: " <> unpack rej
     pure ClientDone
   handleReply (Right (ReplyValue rep)) = do
     putStrLn $ show rep
     case
       withSomeValue TPoint (Proxy @(PipeSpace (SomePipe ()))) rep
       (\(VPoint space) -> do
           traceWith tracer $ "Got pipes.."
           mainWidget $ interact' space
       ) of
       Right x -> x
       Left  e -> fail (unpack e)
     pure ClientDone

interact'
  :: ReflexVty t m
  => PipeSpace (SomePipe ()) -> VtyWidget t m (Event t ())
interact' space = mdo
  inp <- input
  w   <- displayWidth
  h   <- displayHeight

  fromTyD    <- pure . pure .  SomeTypeRep $ typeRep @()
  pipesFromD <- pure $ flip pipesFrom space <$> fromTyD
  descsD     <- pure $ pipesFromD <&>
                       (sortBy (compare `on` somePipeName))

  inputD <- holdDyn (Nothing, 0, "") retE

  visD
    <- pure $ (zipDynWith lcons descsD $ snd . luncons <$> inputD) <&>
       \(descs, col, input) ->
         case parseLocated input of
           Left e  -> ([], somePipeSelectable (3, 3), e)
           Right exp@(indexLocated -> index) ->
             let name = case lookupLocatedQName col index of
                          Nothing -> ""
                          Just qn -> showQName qn
                 xs = infixNameFilter name `Prelude.filter` descs
             in ( xs
                , somePipeSelectable (presentCtx xs)
                , T.pack $ printf "col=%s n=%s exp=%s" (show col) name (show exp))
  retE :: Event t (Maybe (SomePipe ()), Int, Text) <-
    menuSelector
      (DynRegion 0 0 w (h - 6))
      (rpop <$> visD)
      (showName . somePipeName)

  pane (DynRegion 0 (h - 6) w 5) (pure False) $
    richTextStatic (foregro V.red) (thd3 <$> current visD)

  pane (DynRegion 0 (h - 1) w 1) (pure False) $
    richText (RichTextConfig $ pure (foregro V.green))
    (T.pack . show <$> current inputD)

  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing
 where
   presentCtx = (((showName . somePipeName <$>)
                   &&&
                  (showSig  . somePipeSig  <$>))
                  >>>
                 (join (***) (Prelude.maximum . (T.length <$>))))
   infixNameFilter text = isInfixOf text . showName . somePipeName
   escapable w = do
     void w
     i <- input
     return $ fforMaybe i $ \case
       V.EvKey V.KEsc [] -> Just $ Nothing
       _ -> Nothing
   somePipeSelectable
     :: (MonadFix m, MonadHold t m, PostBuild t m, MonadNodeId m, Reflex t)
     => (Int, Int)
     -> SomePipe ()
     -> Behavior t Bool
     -> VtyWidget t m (SomePipe ())
   somePipeSelectable (nameWi, sigWi) sp focusB =
    row $ withSomePipe sp $ \(Pipe pd _) -> do
     fixed (pure nameWi) $
       richText (textFocusStyle (foregro V.green) focusB)
         (pure $ justifyRight nameWi ' ' . showName $ pdName pd)
     fixed 4 $
       richText (textFocusStyle V.defAttr focusB)
         (pure " :: ")
     fixed (pure sigWi) $
       richText (textFocusStyle (foregro V.blue) focusB)
         (pure $ justifyLeft  sigWi ' ' . showSig $ pdSig pd)
     pure sp
    where
      textFocusStyle attr focB =
        RichTextConfig $
          selecting (flip V.withBackColor $ V.rgbColor 1 1 1)
                    (pure attr)
                    focB

   -- look :: PipeSpace (SomePipe ()) -> QName Pipe -> Either Text (SomePipe ())
   -- look space name = lookupSpace name space &
   --   maybeToEither ("No such pipe: " <> showQName name)
   -- loop :: Cons.InputT IO ()
   -- loop = do
   --   minput <- Cons.getInputLine "> "
   --   case minput of
   --     Nothing -> pure ()
   --     Just "quit" -> pure ()
   --     Just input -> do
   --       Cons.outputStrLn $ "Input was: " ++ input
   --       case parse (pack input) of
   --         Left e -> Cons.outputStrLn $ "Parse:  " ++ unpack e
   --         Right exp ->
   --           case compil e opsDesc (pure . look s) exp of
   --             Left e -> Cons.outputStrLn $ "Compile:  " ++ unpack e
   --             Right pipe -> Cons.outputStrLn $ "OK:  " <> show pipe
   --       loop
   -- case (,) (lookupSpace "meta.fromrep" space)
   --            (lookupSpace "meta.unit" space)
   --     of (,) (Just fromrep)
   --            (Just unit) -> do
   --          case apply (app opsDesc) fromrep
   --               (SomeValue
   --                (SomeKindValue
   --                 TPoint (VPoint (Name "Unit" :: Name Type)))) of
   --            Left e -> putStrLn (unpack e)
   --            Right p -> putStrLn (show p)
