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
import qualified Searcher.Data.Result   as Searcher
import qualified NodeEditor.React.Model.Searcher.Hint as Searcher
import Searcher.Data.Result               (Result(..))
import Searcher.Data.Class                          (SearcherData (text), SearcherHint (documentation, prefix))

import Pipe
import Type
import Lift.Types

