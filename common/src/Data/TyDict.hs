{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeInType                 #-}
module Data.TyDict (module Data.TyDict) where

import           Data.Kind                          (Constraint, Type)
import qualified Data.Map                         as Map
import           Data.Proxy                         (Proxy(..))
import           Data.Text                          (Text)
import qualified Data.Vector                      as Vec
import           Type.Reflection                    (SomeTypeRep, Typeable, someTypeRep)


--- In contrast, Data.SOP.Dict is:
--
-- data Dict  (c :: k -> Constraint) (a :: k) where
--    Dict :: c a => Dict c a

data Dict (a :: k) (c :: k -> Constraint) where
  Dict :: c a => Dict a c

data TyDict (c :: Type -> Constraint) where
  TyDict :: (c a, Typeable a) => Proxy a -> TyDict c

type TyDictsRecord c = (Int, Text, SomeTypeRep, TyDict c)

data TyDicts (c :: Type -> Constraint) =
  TyDicts
  { _byRep   :: !(Map.Map SomeTypeRep (TyDictsRecord c))
  , _byName  :: !(Map.Map Text        (TyDictsRecord c))
  , _byIx    :: !(Vec.Vector          (TyDictsRecord c))
  }

type TyDictsLookup k =
  forall (c :: Type -> Constraint). TyDicts c -> k -> Maybe (TyDictsRecord c)

lookupByIx   :: TyDictsLookup Int
lookupByIx   (TyDicts _rep _name ix) k = ix Vec.!? k

lookupByName :: TyDictsLookup Text
lookupByName (TyDicts _rep name _ix) k = k `Map.lookup` name

lookupByRep  :: TyDictsLookup SomeTypeRep
lookupByRep  (TyDicts rep _name _ix) k = k `Map.lookup` rep

empty :: TyDicts c
empty = TyDicts mempty mempty mempty

insert
  :: forall c a. (c a, Typeable a)
  => Text
  -> Proxy a
  -> TyDicts c
  -> TyDicts c
insert name a (TyDicts repM nameM ixV) = TyDicts
  (Map.insert (someTypeRep a) new  repM)
  (Map.insert name            new nameM)
  (Vec.snoc   ixV             new)
 where new :: TyDictsRecord c
       new = (sz, name, someTypeRep a, TyDict a)
       sz  = Vec.length ixV

-- link :: forall c a. (c a, Typeable a) => Proxy a -> (SomeTypeRep, TyDict c)
-- link p = (someTypeRep p, TyDict p)

tyDictReps :: TyDicts c -> [SomeTypeRep]
tyDictReps TyDicts{..} = Map.keys _byRep

tyDictNames :: TyDicts c -> [Text]
tyDictNames TyDicts{..} = Map.keys _byName
