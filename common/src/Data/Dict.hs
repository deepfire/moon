{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeInType                 #-}
module Data.Dict
  ( TyDict(..)
  , TyDicts
  , lookupRep
  , lookupName
  , lookupNameRep
  , empty
  , insert
  , link
  , reps
  , names
  )
where

import           Data.Kind                          (Constraint, Type)
import qualified Data.Map                         as Map
import           Data.Proxy                         (Proxy(..))
import           Data.Text                          (Text)
import           Type.Reflection                    (SomeTypeRep, Typeable, someTypeRep)


--- In contrast, Data.SOP.Dict is:
--
-- data Dict  (c :: k -> Constraint) (a :: k) where
--    Dict :: c a => Dict c a

data TyDict (c :: Type -> Constraint) where
  TyDict :: (c a, Typeable a) => Proxy a -> TyDict c

data TyDicts (c :: Type -> Constraint) =
  TyDicts
  { _byRep  :: (Map.Map SomeTypeRep (TyDict c))
  , _byName :: (Map.Map Text        (TyDict c))
  }

lookupRep :: forall (c :: Type -> Constraint). TyDicts c -> SomeTypeRep -> Maybe (TyDict c)
lookupRep  (TyDicts rep _name) = flip Map.lookup rep

lookupName :: forall (c :: Type -> Constraint). TyDicts c -> Text -> Maybe (TyDict c)
lookupName (TyDicts _rep name) = flip Map.lookup name

lookupNameRep :: forall (c :: Type -> Constraint). TyDicts c -> Text -> Maybe SomeTypeRep
lookupNameRep (TyDicts _rep name) n =
  (\case TyDict (a :: Proxy a) -> someTypeRep a) <$> Map.lookup n name

empty :: TyDicts c
empty = TyDicts mempty mempty

insert
  :: forall c a. (c a, Typeable a)
  => Text
  -> Proxy a
  -> TyDicts c
  -> TyDicts c
insert name a (TyDicts repM nameM) = TyDicts
  (Map.insert (someTypeRep a) (TyDict a)  repM)
  (Map.insert name            (TyDict a) nameM)

link :: forall c a. (c a, Typeable a) => Proxy a -> (SomeTypeRep, TyDict c)
link p = (someTypeRep p, TyDict p)

reps :: TyDicts c -> [SomeTypeRep]
reps (TyDicts reps _) = Map.keys reps

names :: TyDicts c -> [Text]
names (TyDicts _ names) = Map.keys names
