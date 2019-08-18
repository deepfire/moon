{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module NodeEditor.React.Store
    ( module NodeEditor.React.Store
    , module X
) where

import           Common.Prelude              as P hiding (transform)
import           Control.Monad.Trans.Reader
import           NodeEditor.Event.Event     (Event)
import qualified NodeEditor.Event.Event     as Event
import           NodeEditor.React.IsRef
import           NodeEditor.React.Model.App (App)
import           NodeEditor.React.Store.Ref as X
import           React.Flux                 hiding (Event)




instance Typeable a => StoreData (Store a) where
    type StoreAction (Store a) = Event
    transform event store = do
        store ^. sendEvent $ event
        return store

instance Typeable a => IsRef (Ref a) where
    dispatch s a = [SomeStoreAction s (Event.UI a)]

instance HasApp (Store App) where
    app = dt

create :: (StoreData (Store a), MonadIO m) => a -> SendEventM m (Ref a)
create a = do
    se <- ask
    liftIO $ mkStore $ Store a se

createApp :: MonadIO m => App -> SendEvent -> m (Ref App)
createApp app' = runReaderT $ create app'
