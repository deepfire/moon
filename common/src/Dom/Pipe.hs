{-# OPTIONS_GHC -fprint-explicit-kinds -fprint-explicit-foralls -Wno-orphans -Wno-unticked-promoted-constructors #-}
module Dom.Pipe (module Dom.Pipe) where

import GHC.Generics                                 (Generic)

import Basis

import Data.Orphanage ()

import Dom.CTag
import Dom.Error
import Dom.LTag
import Dom.Name
import Dom.Pipe.Constr
import Dom.Sig
import Dom.SomeType
import Dom.Struct
import Dom.Tags
import Dom.VTag


--------------------------------------------------------------------------------
-- | Pipe: component of a computation.  Characterised by:
--   as -- a type-level list of its argument specs, (TypePair (Type c a))
--   o  -- output type, a single 'TypeSpec' type.
--   p  -- pipe's representation:
--           - Dynamic wrapping a function of matching type, or
--           - something else, perhaps simpler (like ()).
data Pipe (l :: Liveness) (as :: [*]) (o :: *) (p :: *) where
  Pipe :: PipeConstr l as o =>
    { pDesc :: !(Desc l as o)
    , p     :: !p
    } -> Pipe l as o p

-- | Everything there is to be said about a pipe,
--   except for its representation.
data Desc (l :: Liveness) (as :: [*]) (o :: *) =
  Desc
  { pdName   :: !(Name Pipe)
  , pdSig    :: !ISig
  , pdStruct :: !Struct
  , pdRep    :: !SomeTypeRep           -- ^ Full type of the pipe.
  , pdLTag   :: !(LTag l)
  , pdArgs   :: !(NP Tags as)
  , pdOut    :: !(Tags o)
  }
  deriving (Generic)

--------------------------------------------------------------------------------
-- * Destructuring
--
pattern PipeD :: ( ArgConstr o
                 , Typeable l
                 , All Typeable as
                 , All IsCTagV (as :: [*])
                 )
              => Name Pipe -> ISig -> Struct -> SomeTypeRep
              -> LTag l
              -> NP Tags as
              -> Tags o
              -> p
              -> Pipe l as o p
-- TODO:  get rid of this, the added benefit is too small.
pattern PipeD name sig str rep live args out p
              <- Pipe (Desc name sig str rep live args out) p

--------------------------------------------------------------------------------
-- * Instances
--
deriving instance Eq  p => Eq  (Pipe l as o p)
deriving instance Ord p => Ord (Pipe l as o p)

instance Functor (Pipe l as o) where
  fmap f (Pipe d x) = Pipe d (f x)

instance Show (Pipe l as o p) where
  show p = "Pipe "<>unpack (showPipe p)

instance Show (Desc l as o) where show = unpack . showDesc
instance (Typeable l, Typeable as, Typeable o) => Read (Desc l as o) where readPrec = failRead

instance Eq (Desc l as o) where
  -- XXX: slightly opportustic, due to omissions.
  Desc ln _ lst lrep _ _ _ == Desc rn _ rst rrep _ _ _ =
    ln == rn && lst == rst && lrep == rrep

instance Ord (Desc l as o) where
  Desc ln _ lst lrep _ _ _ `compare` Desc rn _ rst rrep _ _ _ =
    ln `compare` rn <> lrep `compare` rrep <> lst `compare` rst

instance NFData (Desc l as o) where
  rnf (Desc x y z w _ _ u) = -- XXX: ugh
    rnf x `seq` rnf y `seq` rnf z `seq` rnf w `seq` rnf u

--------------------------------------------------------------------------------
-- * Pipe utils
--
pipeName :: (PipeConstr l as o) => Pipe l as o p -> Name Pipe
pipeName (PipeD name _ _ _ _ _ _ _)   = name
pipeName _ = error "impossible pipeName"

pipeSig :: (PipeConstr l as o) => Pipe l as o p -> ISig
pipeSig   (PipeD _ sig _ _ _ _ _ _)    = sig
pipeSig _ = error "impossible pipeSig"

pipeStruct :: (PipeConstr l as o) => Pipe l as o p -> Struct
pipeStruct   (PipeD _ _ struct _ _ _ _ _) = struct
pipeStruct _ = error "impossible pipeStruct"

pipeRep :: (PipeConstr l as o) => Pipe l as o p -> SomeTypeRep
pipeRep   (PipeD _ _ _ rep _ _ _ _)    = rep
pipeRep _ = error "impossible pipeRep"

pipeArityCase
  :: forall (l :: Liveness) (as :: [*]) (o :: *) (p :: *) (r :: *)
  .  (PipeConstr l as o)
  => Pipe l as o p
  -- Zero.
  -> (forall l' zas' o'
      . (zas' ~ '[], PipeConstr l zas' o')
      => Pipe l' zas' o' p -> r)
  -- One.
  -> (forall l' (a :: *) (sas' :: [*]) o'
      . (sas' ~ (a:'[]), PipeConstr l '[] o', PipeConstr l sas' o'
        , IsCTagV a, IsCTagV (Head sas'))
      => Pipe l' sas' o' p -> r)
  -- Infinity.
  -> (forall l' (a :: *) (nas' :: [*]) (as'' :: [*]) o'
      . (nas' ~ (a:as''), PipeConstr l as'' o', PipeConstr l nas' o')
      => Pipe l' nas' o' p -> r)
  -> r
pipeArityCase p@(Pipe Desc {pdArgs =      Nil} _) z _ _ = z p
pipeArityCase p@(Pipe Desc {pdArgs = _ :* Nil} _) _ s _ = s p
pipeArityCase p@(Pipe Desc {pdArgs = _ :* _}   _) _ _ n = n p

withCompatiblePipes
  :: forall l as1 as2 o1 o2 p a
  .  ( Typeable as1, Typeable as2
     , Typeable o1, Typeable o2)
  => (forall as o. Pipe l as o p -> Pipe l as o p -> a)
  -> Pipe l as1 o1 p
  -> Pipe l as2 o2 p
  -> Maybe a
withCompatiblePipes f l r
  | Just HRefl <- typeRep @as1 `eqTypeRep` typeRep @as2
  , Just HRefl <- typeRep @o1  `eqTypeRep` typeRep @o2
  = Just $ f l r
  | otherwise = Nothing

showPipe, showPipeP :: Pipe l as o p -> Text
showPipe  Pipe{pDesc} = showDesc  pDesc
showPipeP Pipe{pDesc} = showDescP pDesc

pipeOutSomeCTagType ::
  forall l as o co to p
  . ( PipeConstr l as o
    , ReifyCTag co
    , o ~ CTagV co to)
  => Pipe l as o p -> (SomeCTag, SomeTypeRep)
pipeOutSomeCTagType PipeD{} =
  ( SomeCTag $ reifyCTag $ Proxy @co
  , someTypeRep $ Proxy @to)
pipeOutSomeCTagType _ = error "pipeOutSomeCTagType: impossible"

mkNullaryPipeDesc ::
  forall l c v.
  (ReifyCTag c, ReifyVTag v, Typeable c, Typeable v, Typeable (Repr c v))
  => Name Pipe
  -> LTag l
  -> CTag c
  -> VTag v
  -> Desc l '[] (CTagV c v)
mkNullaryPipeDesc n l c v =
  Desc n
       (Sig [] $ I $ tagsSomeType tags)
       mempty
       (someTypeRep $ Proxy @(IO (Fallible (Repr c v))))
       l
       Nil
       tags
 where tags = Tags c v

--------------------------------------------------------------------------------
-- * Desc utils
--
descOutCTag :: Desc l as (CTagV to o) -> CTag to
descOutCTag = tCTag . pdOut

descOutVTag :: Desc l as (CTagV to o) -> VTag o
descOutVTag = tVTag . pdOut

showDesc, showDescP :: Desc l as o -> Text
showDesc  p = pack $ show (pdName p) <>" :: "<>unpack (showSig $ pdSig p)
showDescP = ("("<>) . (<>")") . showDesc
