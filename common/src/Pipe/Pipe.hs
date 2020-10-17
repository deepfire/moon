{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE UndecidableInstances       #-}
module Pipe.Pipe
  ( ArgConstr
  , IsType
  , PipeConstr
  , Pipe(..)
  , pipeSig
  , pipeName
  , pipeStruct
  , pipeRep
  , pipeOutSomeCTagType
  , showPipe
  , showPipeP
  , pipeArityCase
  , Desc(..)
  , descOutCTag
  , descOutVTag
  , showDesc
  , pattern PipeD
  , Sig(..)
  , ISig
  , MSig
  , showSig
  , showSigDotty
  , ListSig(..)
  , toListSig
  , fromListSig
  , Struct(..)
  , Value(..)
  , withCompatiblePipes
  )
where

import qualified Algebra.Graph                    as G
import           Codec.Serialise
import           Data.Dynamic
import qualified Data.Text                        as T
import           GHC.Generics                       (Generic)
import qualified Options.Applicative              as Opt
import           Type.Reflection                    ((:~~:)(..), eqTypeRep)

import Basis
import Dom.CTag
import Dom.Name
import Dom.SomeType
import Dom.Tags
import Dom.Value
import Dom.VTag


--------------------------------------------------------------------------------
-- * cey types
--

-- | Pipe: component of a computation.  Characterised by:
--   c  -- either 'Ground' (wire-transportable) or 'Top' (not so)
--   as -- a type-level list of its argument specs, (TypePair (Type c a))
--   o  -- output type, a single 'TypeSpec' type.
--   p  -- pipe's representation:
--           - Dynamic wrapping a function of matching type, or
--           - something else, perhaps simpler (like ()).
data Pipe (c :: * -> Constraint) (as :: [*]) (o :: *) (p :: *) where
  Pipe :: PipeConstr c as o =>
    { pDesc :: Desc c as o
    , p     :: p
    } -> Pipe c as o p

deriving instance Eq  p => Eq  (Pipe c as o p)
deriving instance Ord p => Ord (Pipe c as o p)

-- | Everything there is to be said about a pipe,
--   except for its representation.
data Desc (c :: * -> Constraint) (cas :: [*]) (o :: *) =
  Desc
  { pdName   :: !(Name Pipe)
  , pdSig    :: !ISig
  , pdStruct :: !Struct
  , pdRep    :: !SomeTypeRep           -- ^ Full type of the pipe.
  , pdArgs   :: !(NP Tags cas)
  , pdOut    :: !(Tags o)
  }
  deriving (Generic)

-- | Sig:  serialisable type signature
type ISig = Sig I
type MSig = Sig Maybe

data Sig f =
  Sig
  { sArgs :: [f SomeType]
  , sOut  :: f SomeType
  }
  deriving (Generic)
deriving instance (Eq  (f SomeType)) => Eq (Sig f)
deriving instance (Ord (f SomeType)) => Ord (Sig f)

newtype ListSig f = ListSig { unListSig :: [f SomeType] }

-- | Struct: Pipe's internal structure,
--   as a graph of type transformations.
newtype Struct =
  Struct (G.Graph SomeType) deriving (Eq, Generic, Ord, Show)

-- * Pipe
--
pattern PipeD :: ( ArgConstr c o
                 , All Typeable cas
                 , All IsType cas
                 , All Top (cas :: [*])
                 )
              => Name Pipe -> ISig -> Struct -> SomeTypeRep
              -> NP Tags cas
              -> Tags o
              -> p
              -> Pipe c cas o p
-- TODO:  get rid of this, the added benefit is too small.
pattern PipeD name sig str rep args out p
              <- Pipe (Desc name sig str rep args out) p

pipeName :: (PipeConstr c as o) => Pipe c as o p -> Name Pipe
pipeName (PipeD name _ _ _ _ _ _)   = name
pipeName _ = error "impossible pipeName"

pipeSig :: (PipeConstr c as o) => Pipe c as o p -> ISig
pipeSig   (PipeD _ sig _ _ _ _ _)    = sig
pipeSig _ = error "impossible pipeSig"

toListSig :: Sig f -> ListSig f
toListSig Sig{..} = ListSig $ sArgs <> [sOut]

fromListSig :: ListSig f -> Maybe (Sig f)
fromListSig = \case
  ListSig [] -> Nothing
  ListSig xs -> Just $ Sig (init xs) (last xs)

pipeStruct :: (PipeConstr c as o) => Pipe c as o p -> Struct
pipeStruct   (PipeD _ _ struct _ _ _ _) = struct
pipeStruct _ = error "impossible pipeStruct"

pipeRep :: (PipeConstr c as o) => Pipe c as o p -> SomeTypeRep
pipeRep   (PipeD _ _ _ rep _ _ _)    = rep
pipeRep _ = error "impossible pipeRep"

pipeArityCase
  :: forall (c :: * -> Constraint) (cas :: [*]) (o :: *) (p :: *) (a :: *)
  .  (PipeConstr c cas o)
  => Pipe c cas o p
  -> (forall
      . (cas ~ '[])
      => Pipe c cas o p -> a)
  -> (forall (ca :: *) (cas' :: [*])
      . (cas ~ (ca:cas'), PipeConstr c cas' o)
      => Pipe c cas o p -> a)
  -> (forall (ca :: *) (cas' :: [*])
      . (cas ~ (ca:cas'), PipeConstr c cas' o, cas' ~ '[])
      => Pipe c cas o p -> a)
  -> a
pipeArityCase p@(Pipe Desc {pdArgs =      Nil} _) nil _ _  = nil p
pipeArityCase p@(Pipe Desc {pdArgs = _ :* Nil} _) _ _ si   = si p
pipeArityCase p@(Pipe Desc {pdArgs = _ :* _}   _) _ cons _ = cons p

withCompatiblePipes
  :: forall c1 c2 as1 as2 o1 o2 p a
  .  ( Typeable as1, Typeable as2
     , Typeable o1, Typeable o2
     , Typeable c1, Typeable c2)
  => (forall c as o. Pipe c as o p -> Pipe c as o p -> a)
  -> Pipe c1 as1 o1 p
  -> Pipe c2 as2 o2 p
  -> Maybe a
withCompatiblePipes f l r
  | Just HRefl <- typeRep @as1 `eqTypeRep` typeRep @as2
  , Just HRefl <- typeRep @o1  `eqTypeRep` typeRep @o2
  , Just HRefl <- typeRep @c1  `eqTypeRep` typeRep @c2
  = Just $ f l r
  | otherwise = Nothing

showPipe, showPipeP :: Pipe c as o p -> Text
showPipe  Pipe{pDesc} = showDesc  pDesc
showPipeP Pipe{pDesc} = showDescP pDesc

pipeOutSomeCTagType ::
  forall c as o co to p
  . ( PipeConstr c as o
    , ReifyCTag co
    , o ~ Types co to)
  => Pipe c as o p -> (SomeCTag, SomeTypeRep)
pipeOutSomeCTagType PipeD{} =
  ( SomeCTag $ reifyCTag $ Proxy @co
  , someTypeRep $ Proxy @to)
pipeOutSomeCTagType _ = error "pipeOutSomeCTagType: impossible"

class    ( Typeable (CTagOf ct), Typeable (TypeOf ct), Typeable ct
         , Top ct
         , ct ~ Types (CTagOf ct) (TypeOf ct)
         ) => IsType (ct :: *)
instance ( Typeable (CTagOf ct), Typeable (TypeOf ct), Typeable ct
         , Top ct
         , ct ~ Types (CTagOf ct) (TypeOf ct)
         ) => IsType (ct :: *)

type ArgConstr (c :: * -> Constraint) (ct :: *)
  = ( IsType ct, Typeable c, ReifyCTag (CTagOf ct), ReifyVTag (TypeOf ct), c (TypeOf ct))

type PipeConstr (c :: * -> Constraint) (cas :: [*]) (o :: *)
  = ( All IsType cas, ArgConstr c o
    , All Typeable cas -- why do we need this, when we have IsType?
    )

instance Functor (Pipe c as o) where
  fmap f (Pipe d x) = Pipe d (f x)

--------------------------------------------------------------------------------
-- * Desc
--
descOutCTag :: Desc c cas (Types to o) -> CTag to
descOutCTag = tCTag . pdOut

descOutVTag :: Desc c cas (Types to o) -> VTag o
descOutVTag = tVTag . pdOut

showDesc, showDescP :: Desc c as o -> Text
showDesc  p = pack $ show (pdName p) <>" :: "<>show (pdSig p)
showDescP = ("("<>) . (<>")") . showDesc

instance Eq (Desc c as o) where
  -- XXX: slightly opportustic, due to omissions.
  Desc ln _ lst lrep _ _ == Desc rn _ rst rrep _ _ =
    ln == rn && lst == rst && lrep == rrep

instance Ord (Desc c as o) where
  Desc ln _ lst lrep _ _ `compare` Desc rn _ rst rrep _ _ =
    ln `compare` rn <> lrep `compare` rrep <> lst `compare` rst

instance NFData (Desc c as o) where
  rnf (Desc x y z w _ u) = -- XXX: ugh
    rnf x `seq` rnf y `seq` rnf z `seq` rnf w `seq` rnf u

--------------------------------------------------------------------------------
-- * Sig
--
showSig :: ISig -> Text -- " ↦ ↣ → ⇨ ⇒ "
showSig (Sig as o) = T.intercalate " → " $ showSomeType False . unI <$> (as <> [o])

showSigDotty :: ISig -> Text -- " ↦ ↣ → ⇨ ⇒ "
showSigDotty (Sig as o) = T.intercalate " → " $ showSomeType True . unI <$> (as <> [o])

instance NFData (f SomeType) => NFData (Sig f)

--------------------------------------------------------------------------------
-- * Struct
--
instance NFData Struct

{-------------------------------------------------------------------------------
  Really boring.
-------------------------------------------------------------------------------}
instance Show (Pipe c as o p) where
  show p = "Pipe "<>unpack (showPipe p)

instance Show (Desc c as o) where show = unpack . showDesc
instance (Typeable c, Typeable as, Typeable o) => Read (Desc c as o) where readPrec = failRead

instance Show ISig where
  show x  =  "("<>T.unpack (showSig x)<>")"
instance (Serialise (f SomeType)) => Serialise (Sig f)
instance Typeable f => Read (Sig f) where readPrec = failRead

instance Serialise Struct
instance Read Struct where readPrec = failRead

