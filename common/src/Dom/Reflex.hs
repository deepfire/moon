module Dom.Reflex (module Dom.Reflex) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Ref

import Data.IORef

import Reflex
import Reflex.Host.Class


-- | The constraints necessary to run a 'LiftServerApp'.
--   See 'runLiftServerWithHandle' for more
--   on why each of these are necessary and how they can be fulfilled.
type MonadReflex t m =
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  , PrimMonad (HostFrame t)
  , ReflexHost t
  , MonadIO (HostFrame t)
  , Ref m ~ IORef
  , Ref (HostFrame t) ~ IORef
  , MonadRef (HostFrame t)
  , NotReady t m
  , TriggerEvent t m
  , PostBuild t m
  , PerformEvent t m
  , MonadIO m
  , MonadIO (Performable m)
  , Adjustable t m
  )
