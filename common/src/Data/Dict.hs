{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeInType                 #-}
module Data.Dict
  ( Dict(..)
  , Dicts
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


data Dict  (c :: Type -> Constraint) =
  forall a. (c a, Typeable a) =>
  Dict (Proxy a)

data Dicts (c :: Type -> Constraint) =
  Dicts
  { _byRep  :: (Map.Map SomeTypeRep (Dict c))
  , _byName :: (Map.Map Text        (Dict c))
  }

lookupRep :: forall (c :: Type -> Constraint). Dicts c -> SomeTypeRep -> Maybe (Dict c)
lookupRep  (Dicts rep _name) = flip Map.lookup rep

lookupName :: forall (c :: Type -> Constraint). Dicts c -> Text -> Maybe (Dict c)
lookupName (Dicts _rep name) = flip Map.lookup name

lookupNameRep :: forall (c :: Type -> Constraint). Dicts c -> Text -> Maybe SomeTypeRep
lookupNameRep (Dicts _rep name) n =
  (\case Dict (a :: Proxy a) -> someTypeRep a) <$> Map.lookup n name

empty :: Dicts c
empty = Dicts mempty mempty

insert
  :: forall c a. (c a, Typeable a)
  => Text
  -> Proxy a
  -> Dicts c
  -> Dicts c
insert name a (Dicts repM nameM) = Dicts
  (Map.insert (someTypeRep a) (Dict a)  repM)
  (Map.insert name            (Dict a) nameM)

link :: forall c a. (c a, Typeable a) => Proxy a -> (SomeTypeRep, Dict c)
link p = (someTypeRep p, Dict p)

reps :: Dicts c -> [SomeTypeRep]
reps (Dicts reps _) = Map.keys reps

names :: Dicts c -> [Text]
names (Dicts _ names) = Map.keys names
