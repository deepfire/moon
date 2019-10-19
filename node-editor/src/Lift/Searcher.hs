module Lift.Searcher where

import Common.Prelude

import qualified Data.Aeson                                 as Aeson
import qualified Data.ByteString.Lazy.Char8                 as BS
import qualified Data.JSString                              as JSString
import qualified Data.Text                                  as Text
import qualified IdentityString                             as IS
import qualified LunaStudio.Data.Searcher.Hint.Library      as Library
import qualified LunaStudio.Data.Searcher.Hint.Class        as Class
import qualified NodeEditor.React.Model.Searcher            as Searcher
import qualified NodeEditor.React.Model.Searcher.Hint       as Hint
import qualified NodeEditor.React.Model.Searcher.Hint.Node  as NodeHint
import qualified NodeEditor.React.Model.Searcher.Input      as Input
import qualified NodeEditor.React.Model.Searcher.Mode       as Mode
import qualified NodeEditor.React.Model.Searcher.Mode.Node  as NodeMode
import qualified NodeEditor.React.Model.Visualization       as Visualization
import qualified NodeEditor.State.Global                    as Global
import qualified Searcher.Data.Class                        as Searcher
import qualified Searcher.Data.Database                     as Database
import qualified Searcher.Data.Result                       as Result
import qualified Searcher.Engine                            as SearcherEngine

import Common.Action.Command              (Command)
import Control.DeepSeq                    (force)
import Data.Ord                           (comparing)
import Data.Set                           (Set)
import Data.Text                          (Text)
import JS.Visualizers                     (sendVisualizationData)
import LunaStudio.Data.PortRef            (OutPortRef)
import LunaStudio.Data.TypeRep            (ConstructorRep (ConstructorRep))
import NodeEditor.Action.Batch            (searchNodes)
import NodeEditor.Action.State.NodeEditor (getLocalFunctions, getSearcher,
                                           inTopLevelBreadcrumb, modifySearcher)
import NodeEditor.State.Global            (State)
import Searcher.Data.Result               (Result)

updateHints :: Command State ()
updateHints = updateHints' >> Lift.Searcher.updateDocumentation

updateHints' :: Command State ()
updateHints' = unlessM inTopLevelBreadcrumb $ do
    pipes <- use Global.pipes
    modifySearcher $ do
        mayQuery <- preuse $ Searcher.input . Input._DividedInput
        let query = fromMaybe def mayQuery
            results = case pipes of
              Nothing -> []
              Just ps -> []
        Searcher.results .= results
        Searcher.waiting .= (pipes == Nothing)
        let selectInput = maybe True (Text.null . view Input.query) mayQuery
        hintsLen <- use $ Searcher.results . to length
        Searcher.selectedPosition .= if selectInput || hintsLen == 0
                                         then Nothing
                                         else Just 0

-- search :: Input.Divided -> NodeHint.Database
--        -> Maybe Class.Name -> [Result Hint.Hint]
-- search input nsData mayClassName =
--     if Text.strip (input ^. Input.prefix) == "def"
--         then mempty
--         else fullDbSearch input localDb nsData mayClassName

-- fullDbSearch :: Input.Divided -> NodeHint.Database
--              -> Maybe Class.Name -> [Result Hint.Hint]
-- fullDbSearch input nsData mayClassName = let
--     query            = input ^. Input.query
--     nextSym          = input ^. Input.nextSymbolPrediction
--     scoredGlobal     = scoreTextMatch query nsData
--     semanticGlobal   = bumpGlobalSyms nextSym mayClassName scoredGlobal
--     filteredSnippets = filterSnippets input mayClassName semanticGlobal
--     scoredSnippets   = bumpSnippets filteredSnippets
--     scoredImports    = bumpImported scoredSnippets
--     allHints         = semanticLocal <> scoredImports
--     scoredPriority   = scorePriority <$> allHints
--     sorted = sortBy (comparing $ negate . view Result.score) scoredPriority
--     in Hint.Node <<$>> sorted

updateDocumentation :: Command State ()
updateDocumentation = withJustM getSearcher $ \s -> do
    let mayDocVis = s ^? Searcher.documentationVisualization
        mayDoc = s ^? Searcher.selectedResult . _Just . Searcher.documentation
        mayDocData = (,) <$> mayDocVis <*> mayDoc
    withJust mayDocData $ \(docVis, doc) -> liftIO $ sendVisualizationData
        (docVis ^. Visualization.visualizationId)
        (ConstructorRep "Text" def)
        =<< (IS.fromJSString . JSString.pack . BS.unpack $ Aeson.encode doc)

clearHints :: Command State ()
clearHints = do
    modifySearcher $ do
        Searcher.selectedPosition .= def
        Searcher.results          .= mempty
    updateDocumentation
