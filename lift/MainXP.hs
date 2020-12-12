import Reflex                             hiding (Request)
import Reflex.Network
import Reflex.Vty                         hiding (Request)

import Data.Text qualified                     as T
import Graphics.Vty qualified                  as V
import Reflex.Vty.Widget.Extra
import Reflex.Vty.Widget.Selector
import Graphics.Vty qualified                  as V

import Reflex.Vty.Widget.Extra
import Reflex.Vty.Widget.Selector

import Basis hiding (Dynamic)
import Debug.Reflex

main :: IO ()
main = do
  mainWidget reflexVtyApp

reflexVtyApp :: forall t m.
  (MonadIO (Performable m), ReflexVty t m, PerformEvent t m, PostBuild t m, TriggerEvent t m, MonadIO m)
  => VtyWidget t m (Event t ())
reflexVtyApp = do
  nowE <- now
  txt <- hold "Nothing" $ "Just woot" <$ trevs "nowE!" nowE
  splitV (pure $ \x->x-8) (pure $ join (,) True)
         (richTextStatic blue $ pure "blue")
         (richTextStatic red  $ txt)
  exitOnTheEndOf input
