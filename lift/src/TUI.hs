{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

module TUI (tui) where

import           Codec.Serialise
import           Control.Tracer
import           Data.Binary
import qualified Data.ByteString.Lazy           as LBS
import           Data.Maybe
import           Data.Text
import qualified Network.WebSockets             as WS
import           Options.Applicative hiding (str)
import           Options.Applicative.Common

import Basis
import Ground.Hask
import Lift                   hiding (Config)
import Wire.Peer
import Wire.Protocol

import           Brick
import           Brick.AttrMap
import           Brick.BChan
import           Brick.Types
import           Brick.Widgets.Border
import           Brick.Widgets.Core
import           Brick.Widgets.List
import           Brick.Widgets.List
import           Control.Comonad
import           Control.Comonad.Cofree        as CF
import           Control.Comonad.Cofree        as CF
import           Control.Concurrent.STM (STM, atomically)
import           Control.Concurrent.STM.TBQueue as Q
import           Data.Bool
import           Data.Foldable
import           Data.List
import           Graphics.Vty hiding (Config)
import qualified Graphics.Vty as V
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Data.Sequence as S
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Vector as V

data Addr = Addr
  { aHost :: String
  , aPort :: Int
  }

data Connection rej = Connection
  { cAddr :: !Addr
  , cRequests :: !(Q.TBQueue SomeRequest)
  , cReplies :: !(Q.TBQueue (Either rej SomeReply))
  , cAsync :: Async.Async ()
  }

request
  :: (Serialise rej, Show rej, rej ~ Text)
  => Connection rej
  -> SomeRequest
  -> IO (Either rej SomeReply)
request c req = atomically $ do
  Q.writeTBQueue (cRequests c) req
  Q.readTBQueue (cReplies c)

connect
  :: (Serialise rej, Show rej, rej ~ Text)
  => Addr
  -> IO (Connection rej)
connect addr = mdo
  conn <- Connection
    <$> pure addr
    <*> Q.newTBQueueIO 10
    <*> Q.newTBQueueIO 10
    <*> (Async.async $ runConnection conn)
  pure conn

runConnection
  :: (Serialise rej, Show rej)
  => Connection rej
  -> IO ()
runConnection c@(Connection a reqs reps _) =
  WS.runClient (aHost a) (aPort a) "/" $
    \conn -> do
      let tracer = stdoutTracer
          -- req = fromMaybe (SomeRequest $ RunPipe "pipes" []) mreq
      runClient tracer (client tracer c) (channelFromWebsocket conn)

client
  :: forall rej m a
  . (m ~ IO, a ~ SomeReply)
  => Tracer m String
  -> Connection rej
  -> m (ClientState rej m a)
client tracer c = loop
  where
    loop :: m (ClientState rej IO SomeReply)
    loop = do
      req <- atomically $ Q.readTBQueue (cRequests c)
      pure $ ClientRequesting req handleReply
    handleReply :: Either rej a -> IO (ClientState rej m a)
    handleReply (Left rej) = do
      atomically $ Q.writeTBQueue (cReplies c) (Left rej)
      loop --pure SendMsgDone
    handleReply (Right rep) = do
      putStrLn $ show rep
      atomically $ Q.writeTBQueue (cReplies c) (Right rep)
      loop

-- readDirectoryWithL :: (FilePath -> IO a) -> FilePath -> IO (AnchoredDirTree a)
tui :: IO ()
tui = do
  mreq <- execParser $ (info $ (optional parseSomeRequest) <**> helper) fullDesc
  conn <- connect (Addr "127.0.0.1" (cfWSPortOut Lift.defaultConfig))

  -- ft <- newFileTree (const pure)
  -- eChan <- newBChan 10
  -- let mkVty' = mkVty Graphics.Vty.defaultConfig
  -- vty <- mkVty'
  -- state <- customMain vty mkVty'
  --   (Just eChan)
  --   (undefined :: App () () ())
  --   () -- state
  putStrLn "EOF"

-- data AppState =
--   AppState
--     { fileTree     :: FileTree FilePath
--     , eventChannel :: BChan DelveEvent
--     , prompt       :: Maybe (Editor String String, CmdInputHandler)
--     , status       :: String
--     }

-- app :: App AppState DelveEvent ResourceName
-- app =
--   App
--   { appDraw         = drawUI
--   , appChooseCursor = chooseCursor
--   , appHandleEvent  = interceptPromptEvents
--   , appStartEvent   = pure
--   , appAttrMap      = const attrs
-- }

-- drawUI :: AppState -> [Widget ResourceName]
-- drawUI s =
--   [ renderPrompt (prompt s)
--   , renderFileTreeCustom renderFileContext (s ^. _fileTree)
--       <=> hBorder
--       <=> str (status s)
--   ]

-- handleEvent :: BrickEvent ResourceName DelveEvent
--             -> AppState
--             -> EventM ResourceName (Next AppState)
-- handleEvent (VtyKey 'c' [MCtrl]) = halt
-- handleEvent (VtyKey 'q' []) = halt
-- handleEvent (VtyKey '-' []) = continue . (_fileTree %~ toggleFlaggedVisible)
-- handleEvent (VtyKey 'l' []) = _fileTree %%~ descendDir >=> continue
-- handleEvent (VtyKey ' ' []) = _fileTree %%~ toggleFlagged >=> continue
-- handleEvent (VtyEvent (EvKey KEnter _)) =
--   \s -> do
--     openInVim (s ^. _fileTree)
--     continue s
-- handleEvent (VtyKey 'h' []) = _fileTree %%~ ascendDir >=> continue
-- handleEvent (VtyKey 'j' []) = _fileTree %%~ moveDown >=> continue
-- handleEvent (VtyKey 'k' []) = _fileTree %%~ moveUp >=> continue
-- handleEvent (VtyKey 'o' []) =
--   \s -> do
--     void . liftIO
--       $ spawnCmd
--         "scr"
--         (treeContext . fileTree $ s)
--         (responseToChannel (eventChannel s))
--     continue s
--   where
--     responseToChannel :: BChan DelveEvent -> CmdOutputHandler
--     responseToChannel eChan responder respStr =
--       do
--         appendFile "log" (respStr ++ "\n")
--         writeBChan eChan (SpawnPrompt "scr" respStr responder)

--     treeContext ft =
--       M.fromList
--         [ (flaggedKey, unlines $ getFlagged ft)
--         , (focusedKey, fromMaybe "" $ getCurrentFilePath ft)
--         , (currentDirKey, getCurrentDir ft)
--         ]
-- handleEvent (AppEvent e) = (_prompt %%~ handleScriptEvents e) >=> continue
-- handleEvent _ = continue

-- newFileTree :: ValueLoader a -> IO (FileTree a)
-- newFileTree valLoader' currentDir = do
--   (_ FT.:/ tree) <- FT.readDirectoryWithL (interleavedValLoader File) absRoot
--   convert interleavedValLoader (takeDirectory absRoot) tree
--   where interleavedValLoader fk fp = unsafeInterleaveIO $ valLoader' fk fp

-- convert
--   :: forall a . ValueLoader a -> FilePath -> FT.DirTree a -> IO (FileTree a)
-- convert valLoader' root tree = do
--   subTree <- go (normalise root) tree
--   pure $ FT
--     { parents   = []
--     , selection = mempty
--     , config    = defaultConfig
--     , context   = subTree
--     , valLoader = valLoader'
--     }
--  where
--   go :: FilePath -> FT.DirTree a -> IO (SubTree a)
--   go root' (FT.Failed { FT.name, FT.err }) = do
--     val <- valLoader' Error name
--     pure
--       $  FC
--            { name    = show err
--            , path    = normalise (root' </> name)
--            , flagged = False
--            , kind    = Error
--            , val     = val
--            }
--       :< list name mempty 1
--   go root' (FT.File { FT.name, FT.file }) =
--     pure
--       $  FC
--            { name    = name
--            , path    = normalise (root' </> name)
--            , flagged = False
--            , kind    = File
--            , val     = file
--            }
--       :< list name mempty 1
--   go root' (FT.Dir path contents) = do
--     let absPath = normalise (root' </> path)
--     val      <- valLoader' Dir absPath
--     children <- traverse (go absPath) contents
--     pure
--       $  FC
--            { name    = path <> "/"
--            , path    = absPath
--            , kind    = Dir
--            , flagged = False
--            , val     = val
--            }
--       :< list path (V.fromList . sortOn byFileType $ children) 1

-- data FileTree a = FT
--   { parents :: S.Seq (SubTree a)
--   , selection :: S.Set FilePath
--   , context :: SubTree a
--   , config :: Config
--   , valLoader :: ValueLoader a
--   }

-- type ValueLoader a = FileKind -> FilePath -> IO a

-- data Config =
--   Config
--     { showSelection :: Bool
--     , previewDir :: Bool
--     }

-- type SubTree a = Cofree (GenericList String V.Vector) (FileContext a)

-- data FileContext a =
--   FC
--     { flagged :: Bool
--     , path :: FilePath
--     , name :: String
--     , kind :: FileKind
--     , val :: a
--     }

-- data FileKind = Dir | File | Error
--   deriving (Eq, Ord, Show)

-- -- | Custom rendering function for a file
-- -- Note that the resulting widget must be exactly 1 row high.
-- type CustomFCRender a = FileContext a -> Widget String

-- -- | Flagged items are rendered with this attr
-- flaggedItemAttr :: AttrName
-- flaggedItemAttr = "flaggedItemAttr"
-- -- | UI Titles have this attr
-- titleAttr :: AttrName
-- titleAttr = "titleAttr"
-- -- | Directories in the list have this attr
-- dirAttr :: AttrName
-- dirAttr = "dirAttr"
-- -- | Files in the list have this attr
-- fileAttr :: AttrName
-- fileAttr = "fileAttr"
-- -- | Errors have this attr
-- errorAttr :: AttrName
-- errorAttr = "errorAttr"

-- cacheKey :: FileContext a -> String
-- cacheKey = path

-- renderHeader :: SubTree a -> Widget String
-- renderHeader ((path -> p) :< _) =
--   withAttr titleAttr (str $ p <> "/") <=> hBorder

-- renderFileTreeCustom :: CustomFCRender a -> FileTree a -> Widget String
-- renderFileTreeCustom customFCRender fz@(FT { parents, context, config }) =
--   (   renderHeader context
--   <=> (   renderParents customFCRender parents
--       <+> renderNode customFCRender True context
--       <+> previewW
--       )
--   <=> selectionW
--   )
--  where
--   selectionW = if showSelection config then renderSelection fz else emptyWidget
--   previewW   = if previewDir config
--     then renderPreview customFCRender context
--     else emptyWidget

-- renderFileTree :: FileTree a -> Widget String
-- renderFileTree = renderFileTreeCustom renderFileContext

-- renderPreview :: CustomFCRender a -> SubTree a -> Widget String
-- renderPreview customFCRender (_ :< lst) = do
--   case listSelectedElement lst of
--     Just (_, node@(FC { kind = Dir } :< _)) ->
--       vBorder <+> renderNode customFCRender False node
--     _ -> emptyWidget

-- selectionCacheKey :: String
-- selectionCacheKey = "delve!selection"

-- renderSelection :: FileTree a -> Widget String
-- renderSelection (FT { selection })
--   | Data.List.null selection
--   = emptyWidget
--   | otherwise
--   = let selectionsW =
--           cached selectionCacheKey
--             . vBox
--             . fmap (withAttr flaggedItemAttr . str)
--             . toList
--             $ selection
--     in  hBorder <=> withAttr titleAttr (str "flagged") <=> selectionsW

-- renderParents :: CustomFCRender a -> S.Seq (SubTree a) -> Widget String
-- renderParents _              S.Empty                    = emptyWidget
-- renderParents customFCRender parents@(_ S.:|> (p :< _)) = cached
--   (cacheKey p)
--   (hBox . toList $ (renderParent customFCRender <$> Seq.drop ind parents))
--  where
--   len = S.length parents
--   ind = max 0 (len - 2)

-- renderNode :: CustomFCRender a -> Bool -> SubTree a -> Widget String
-- renderNode customFCRender focused (_ :< ls) = renderList
--   (\b -> bool id (forceAttr listSelectedAttr) b . customFCRender . extract)
--   focused
--   ls

-- renderParent :: CustomFCRender a -> SubTree a -> Widget String
-- renderParent customFCRender =
--   (<+> vBorder) . hLimit 20 . renderNode customFCRender False

-- renderFileContext :: FileContext a -> Widget String
-- renderFileContext (FC { kind = File, name, flagged }) =
--   let (attr', modStr) =
--         if flagged then (flaggedItemAttr, "* ") else (fileAttr, "")
--   in  withAttr attr' . str $ modStr <> name
-- renderFileContext (FC { kind = Error, name, path }) =
--   withAttr errorAttr . str $ "! " <> path <> ": " <> name
-- renderFileContext (FC { kind = Dir, name, flagged }) =
--   let (attr', modStr) =
--         if flagged then (flaggedItemAttr, "* ") else (dirAttr, "")
--   in  withAttr attr' . str $ modStr <> name

-- overCurrentList
--   :: (List String (SubTree a) -> EventM String (List String (SubTree a)))
--   -> FileTree a
--   -> EventM String (FileTree a)
-- overCurrentList f fz@(FT { context = x :< lst }) = do
--   newLst <- f lst
--   return fz { context = x :< newLst }

-- pressKey :: V.Key -> (FileTree a -> EventM String (FileTree a))
-- pressKey k = overCurrentList (handleListEvent (V.EvKey k []))

-- -- | Move the cursor down one item
-- moveDown :: FileTree a -> EventM String (FileTree a)
-- moveDown = pressKey V.KDown

-- -- | Move the cursor up one item
-- moveUp :: FileTree a -> EventM String (FileTree a)
-- moveUp = pressKey V.KUp

-- -- | Move the cursor down a page
-- pageDown :: FileTree a -> EventM String (FileTree a)
-- pageDown = pressKey V.KPageDown

-- -- | Move the cursor up a page
-- pageUp :: FileTree a -> EventM String (FileTree a)
-- pageUp = pressKey V.KPageDown

-- -- | Move the cursor the the top of the file list
-- moveToTop :: FileTree a -> EventM String (FileTree a)
-- moveToTop = pressKey V.KHome

-- -- | Move the cursor the the bottom of the file list
-- moveToBottom :: FileTree a -> EventM String (FileTree a)
-- moveToBottom = pressKey V.KEnd

-- -- | Move the cursor up a directory in the file tree
-- ascendDir :: FileTree a -> EventM String (FileTree a)
-- ascendDir (FT { parents = Seq.Empty, context = tree@((extract -> path -> p)), selection, valLoader, ..})
--   = do
--     fz <- liftIO $ buildParent p valLoader tree
--     return $ fz { selection = selection }
-- ascendDir (FT { parents = (ps Seq.:|> (f :< pList)), context, ..}) = do
--   invalidateCacheEntry (cacheKey f)
--   return
--     $ FT {parents = ps, context = (f :< listModify (const context) pList), ..}

-- -- | If the cursor is on a directory then descend the cursor into that dir
-- -- If the cursor is on a file nothing happens
-- descendDir :: FileTree a -> EventM String (FileTree a)
-- descendDir fz@(FT { parents, context = (f :< children), ..}) = do
--   invalidateCacheEntry (cacheKey f)
--   return $ case listSelectedElement children of
--     Nothing -> fz
--     Just (_, nextChildren@(FC { kind = Dir } :< _)) -> FT
--       { parents = (parents Seq.|> (f :< children))
--       , context = nextChildren
--       , ..
--       }
--     Just _ -> fz

-- -- | Get the absolute path of the object (dir or file) under the cursor
-- getCurrentFilePath :: FileTree a -> Maybe FilePath
-- getCurrentFilePath (FT { context = unwrap -> children }) =
--   case listSelectedElement children of
--     Nothing                            -> Nothing
--     Just (_, FC { kind = Error } :< _) -> Nothing
--     Just (_, fc :< _                 ) -> Just (path fc)

-- -- | Get the absolute path of the directory where the cursor currently is.
-- getCurrentDir :: FileTree a -> FilePath
-- getCurrentDir (FT { context = extract -> path -> p }) = p

-- -- | Flag or unflag the current file or dir
-- toggleFlagged :: FileTree a -> EventM String (FileTree a)
-- toggleFlagged fz@(FT { context = (fc :< lst), selection, ..}) = do
--   invalidateCacheEntry selectionCacheKey
--   return . fromMaybe fz $ do
--     ((selectedContext@FC { flagged = isSelected, path }) :< rest) <- snd
--       <$> listSelectedElement lst
--     let newSelection = if isSelected
--           then S.delete path selection
--           else S.insert path selection
--     let newList = listModify
--           (const (selectedContext { flagged = not isSelected } :< rest))
--           lst
--     return $ FT {context = (fc :< newList), selection = newSelection, ..}

-- -- | Get all flagged file paths. All paths are absolute
-- getFlagged :: FileTree a -> [FilePath]
-- getFlagged = toList . selection

-- -- | Hide/Show a list of all flagged files
-- toggleFlaggedVisible :: FileTree a -> FileTree a
-- toggleFlaggedVisible fz@(FT { config }) =
--   fz { config = config { showSelection = not $ showSelection config } }
