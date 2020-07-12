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
  , pipeOutSomeTagType
  , showPipe
  , showPipeP
  , pipeArityCase
  , Desc(..)
  , descOutTag
  , descOutType
  , showDesc
  , pattern PipeD
  , Sig(..)
  , showSig
  , showSigDotty
  , ListSig(..)
  , toListSig
  , Struct(..)
  , Value(..)
  , withCompatiblePipes
  )
where

import qualified Algebra.Graph                    as G
import           Codec.Serialise
import           Data.Dynamic
import qualified Data.Text                        as T
import           Type.Reflection                    ((:~~:)(..), eqTypeRep)
import           GHC.Generics                       (Generic)

import Basis
import Type
import SomeType


--------------------------------------------------------------------------------
-- * Key types
--

-- | Pipe: component of a computation.  Characterised by:
--   c  -- either 'Ground' (wire-transportable) or 'Top' (not so)
--   as -- a type-level list of its argument specs, (TypePair (Type k a))
--   o  -- output type, a single 'TypeSpec' type.
--   p  -- pipe's representation:
--           - Dynamic wrapping a function of matching type, or
--           - something else, perhaps simpler (like ()).
data Pipe (c :: * -> Constraint) (as :: [*]) (o :: *) (p :: *) where
  Pipe :: PipeConstr c as o =>
    { pDesc :: Desc c as o
    , p     :: p
    } -> Pipe c as o p

-- | Everything there is to be said about a pipe,
--   except for its representation.
data Desc (c :: * -> Constraint) (kas :: [*]) (o :: *) =
  Desc
  { pdName   :: !(Name Pipe)
  , pdSig    :: !Sig
  , pdStruct :: !Struct
  , pdRep    :: !SomeTypeRep -- ^ Full type of the pipe, App4-style.
  , pdArgs   :: !(NP TypePair kas)
  , pdOut    :: !(TypePair o)
  }
  deriving (Generic)

-- | Sig:  serialisable type signature
data Sig =
  Sig
  { sArgs :: [SomeType]
  , sOut  :: SomeType
  }
  deriving (Eq, Generic, Ord)

newtype ListSig = ListSig { unListSig :: [SomeType] }

-- | Struct: Pipe's internal structure,
--   as a graph of type transformations.
newtype Struct =
  Struct (G.Graph SomeType) deriving (Eq, Generic, Ord, Show)

-- * Pipe
--
pattern PipeD :: ( ArgConstr c o
                 , All Typeable kas
                 , All IsType kas
                 , All Top (kas :: [*])
                 )
              => Name Pipe -> Sig -> Struct -> SomeTypeRep
              -> NP TypePair kas
              -> TypePair o
              -> p
              -> Pipe c kas o p
-- TODO:  get rid of this, the added benefit is too small.
pattern PipeD name sig str rep args out p
              <- Pipe (Desc name sig str rep args out) p

pipeName :: (PipeConstr c as o) => Pipe c as o p -> Name Pipe
pipeName (PipeD name _ _ _ _ _ _)   = name
pipeName _ = error "impossible pipeName"

pipeSig :: (PipeConstr c as o) => Pipe c as o p -> Sig
pipeSig   (PipeD _ sig _ _ _ _ _)    = sig
pipeSig _ = error "impossible pipeSig"

toListSig :: Sig -> ListSig
toListSig Sig{..} = ListSig $ sArgs <> [sOut]

pipeStruct :: (PipeConstr c as o) => Pipe c as o p -> Struct
pipeStruct   (PipeD _ _ struct _ _ _ _) = struct
pipeStruct _ = error "impossible pipeStruct"

pipeRep :: (PipeConstr c as o) => Pipe c as o p -> SomeTypeRep
pipeRep   (PipeD _ _ _ rep _ _ _)    = rep
pipeRep _ = error "impossible pipeRep"

pipeArityCase
  :: forall (c :: * -> Constraint) (kas :: [*]) (o :: *) (p :: *) (a :: *)
  .  (PipeConstr c kas o)
  => Pipe c kas o p
  -> (forall
      . (kas ~ '[])
      => Pipe c kas o p -> a)
  -> (forall (ka :: *) (kas' :: [*])
      . (kas ~ (ka:kas'), PipeConstr c kas' o)
      => Pipe c kas o p -> a)
  -> (forall (ka :: *) (kas' :: [*])
      . (kas ~ (ka:kas'), PipeConstr c kas' o, kas' ~ '[])
      => Pipe c kas o p -> a)
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

pipeOutSomeTagType ::
  forall c as o ko to p
  . ( PipeConstr c as o
    , ReifyTag ko
    , o ~ Type ko to)
  => Pipe c as o p -> (SomeTag, SomeTypeRep)
pipeOutSomeTagType PipeD{} =
  ( SomeTag $ reifyTag $ Proxy @ko
  , someTypeRep $ Proxy @to)
pipeOutSomeTagType _ = error "pipeOutSomeTagType: impossible"

class    ( Typeable (TagOf ct), Typeable (TypeOf ct), Typeable ct
         , Top ct
         , ct ~ Type (TagOf ct) (TypeOf ct)
         ) => IsType (ct :: *)
instance ( Typeable (TagOf ct), Typeable (TypeOf ct), Typeable ct
         , Top ct
         , ct ~ Type (TagOf ct) (TypeOf ct)
         ) => IsType (ct :: *)

type ArgConstr (c :: * -> Constraint) (ct :: *)
  = ( IsType ct, Typeable c, ReifyTag (TagOf ct), c (TypeOf ct))

type PipeConstr (c :: * -> Constraint) (kas :: [*]) (o :: *)
  = ( All IsType kas, ArgConstr c o
    , All Typeable kas -- why do we need this, when we have IsType?
    )

instance Eq (Pipe c as o ()) where
  -- XXX: potentially problematic instance
  (==) = (==) `on` pDesc

instance Ord (Pipe c as o ()) where
  -- XXX: potentially problematic instance
  compare = compare `on` pDesc

instance Functor (Pipe c as o) where
  fmap f (Pipe d x) = Pipe d (f x)

--------------------------------------------------------------------------------
-- * Desc
--
descOutTag :: Desc c kas (Type to o) -> Tag to
descOutTag = tpTag . pdOut

descOutType :: Desc c kas (Type to o) -> Proxy o
descOutType = tpType . pdOut

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
showSig :: Sig -> Text -- " ↦ ↣ → ⇨ ⇒ "
showSig (Sig as o) = T.intercalate " ⇨  " $ showSomeType False <$> (as <> [o])

showSigDotty :: Sig -> Text -- " ↦ ↣ → ⇨ ⇒ "
showSigDotty (Sig as o) = T.intercalate " ⇨  " $ showSomeType True <$> (as <> [o])

instance NFData Sig

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

instance Show Sig where
  show x  =  "("<>T.unpack (showSig x)<>")"
instance Serialise Sig
instance Read Sig where readPrec = failRead

instance Serialise Struct
instance Read Struct where readPrec = failRead

