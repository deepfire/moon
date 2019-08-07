module NodeEditor.Handler.App
    ( handle
    ) where

import           Common.Prelude
import           Common.Report  (warning)
import qualified Data.UUID as UUID
import qualified Data.Map.Lazy                               as Map

import qualified JS.Visualizers                              as JSVis

import qualified LunaStudio.Data.NodeLoc as LSLoc
import           LunaStudio.Data.NodeMeta
import           LunaStudio.Data.Position
import qualified LunaStudio.Data.Breadcrumb as LSBread
import qualified LunaStudio.Data.Visualizer as LSVis

import           Common.Action.Command          (Command)

import           NodeEditor.Action.Basic        (setFile, selectNode, unselectAll, unsetFile, updateFilePath, updateScene, localAddExpressionNode, localSetNodeMeta)
import           NodeEditor.Action.State.Action (checkIfActionPerfoming, endActions, endAllActions)
import qualified NodeEditor.Action.Batch        as Batch
import qualified NodeEditor.Action.Port         as PortControl
import qualified NodeEditor.Action.State.NodeEditor as State

import           NodeEditor.Event.Event         (Event (Atom, Init, Shortcut, UI))
import           NodeEditor.Event.UI            (UIEvent (AppEvent, SidebarEvent))
import qualified NodeEditor.Event.Atom          as Atom
import qualified NodeEditor.Event.Shortcut      as Shortcut

import qualified NodeEditor.React.Event.App     as App
import qualified NodeEditor.React.Event.Sidebar as Sidebar
import qualified NodeEditor.React.Model.NodeEditor           as NE
import qualified NodeEditor.React.Model.Visualization        as Vis
import qualified NodeEditor.React.Model.Node.ExpressionNode  as Node

import           NodeEditor.State.Action        (Action (continue), ActionRep, textPortControlEditAction)
import           NodeEditor.State.Action        (actionsClosingOnMouseLeave)
import           NodeEditor.State.Global        (State)
import           NodeEditor.State.Mouse         (mousePosition)

import qualified NodeEditor.State.Global        as Global
import qualified NodeEditor.State.UI            as UI

import           React.Flux                     ( KeyboardEvent(..)
                                                , MouseEvent(..)
                                                , WheelEvent)

updateVisualizers :: Command State ()
updateVisualizers = do
    pcv <- State.getPlaceholderVisualizer
    let externalVisPaths = Vis.ExternalVisualizers Nothing mempty
    internalVisPath <- liftIO $ JSVis.getInternalVisualizersLibraryPath
    lunaVisPath     <- liftIO $ JSVis.getLunaVisualizersLibraryPath
    State.modifyNodeEditor $ NE.visualizersLibPaths
        .= Vis.Visualizers internalVisPath lunaVisPath externalVisPaths
    let toMatcherMap tpe = Map.mapKeys (Vis.VisualizerId tpe) . LSVis.fromJSVisualizersMap
    internalVisMap <- liftIO
        $ LSVis.fromJSInternalVisualizersMap <$> JSVis.mkInternalVisualizersMap
    lunaVisMap <- liftIO
        $ toMatcherMap Vis.LunaVisualizer <$> JSVis.mkLunaVisualizersMap
    Global.visualizers .= lunaVisMap
    Global.internalVisualizers .= internalVisMap
    phv <- State.getPlaceholderVisualizer
    liftIO $ warn "phv" (show phv)

play :: Command Global.State ()
play = do
  --liftIO $ warn "keydown" (show ev)
  updateVisualizers
  --
  let bc  = LSBread.Breadcrumb [LSBread.Definition (UUID.nil)]
      nl  = LSLoc.fromPath (LSLoc.NodePath bc)
      pos = fromTuple (300, 300)
      n   = Node.mkExprNode nl "memptee" pos
            & Node.name               .~ Just "wootwoot"
            & Node.code               .~ "yowza"
            & Node.visEnabled         .~ True
            & Node.errorVisEnabled    .~ True
  localAddExpressionNode n
  selectNode nl
  State.setGraphStatus NE.GraphLoaded
  all <- State.getAllNodes
  liftIO $ warn "all" (show all)

handle :: Event -> Maybe (Command Global.State ())
handle (UI (AppEvent     (ev@(App.KeyDown (KeyboardEvent{keyKey="F2"}))))) = Just play

handle (UI (AppEvent     (App.MouseMove evt _)))       = Just $ Global.ui . UI.mousePos <~ mousePosition evt
handle (UI (SidebarEvent (Sidebar.MouseMove evt _ _))) = Just $ Global.ui . UI.mousePos <~ mousePosition evt
handle (UI (AppEvent     App.Resize          ))        = Just $ do
  liftIO $ warn "handle" (show App.Resize)
  updateScene
handle (UI (AppEvent     App.MouseLeave      ))        = Just $ endActions actionsClosingOnMouseLeave
handle (Shortcut         (Shortcut.Event command _))   = Just $ handleCommand command
handle  Init                                           = Just $ do
  liftIO $ warn "handle" (show Init)
  Batch.getProgram def True
  play
handle (Atom (Atom.SetFile path))                      = Just $ setFile path
handle (Atom (Atom.UpdateFilePath path))               = Just $ updateFilePath path
handle (Atom  Atom.UnsetFile)                          = Just   unsetFile
handle _                                               = Nothing


cancelAllActions :: Command State [ActionRep]
cancelAllActions = do
    tpcePerforming <- checkIfActionPerfoming textPortControlEditAction
    if not tpcePerforming then endAllActions else do
        continue PortControl.rollbackEditTextPortControl
        PortControl.unfocusEditTextPortControl
        (textPortControlEditAction :) <$> endAllActions

handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.Cancel -> whenM (null <$> cancelAllActions) unselectAll
    _               -> return ()
