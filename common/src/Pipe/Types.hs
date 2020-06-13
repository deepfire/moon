{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# OPTIONS_GHC -fprint-explicit-kinds -fprint-explicit-foralls -Wno-orphans -Wno-unticked-promoted-constructors #-}
module Pipe.Types
  ( SomePipeSpace
  , PipeSpace(..)
  , SomePipeScope
  , Ops(..)
  , PipeOps(..)
  , SomePipe(..)
  , somePipeName
  , somePipeSig
  , withSomePipe
  , pipeArityCase
  , somePipeUncons
  , ArgConstr
  , IsType
  , PipeConstr
  , PipeFunTy
  -- , PipePairFunTy
  , Result
  , Pipe(..)
  , pipeSig
  , pipeName
  , pipeStruct
  , pipeRep
  , somePipeRep
  , showPipe
  , showPipeP
  , showLR
  , showLRP
  , Desc(..)
  , descOutTag
  , descOutType
  , showDesc
  , pattern PipeD
  , Sig(..)
  , showSig
  , Struct(..)
  , Value(..)
  , withCompatiblePipes
  )
where

import qualified Algebra.Graph                    as G
import           Codec.Serialise
import           Codec.CBOR.Encoding                (encodeListLen, encodeWord)
import           Codec.CBOR.Decoding                (decodeListLen, decodeWord)
import           Data.Dynamic
import qualified Data.SOP                         as SOP
import qualified Data.Text                        as T
import           Type.Reflection                    ((:~~:)(..), eqTypeRep)
import qualified Data.Map.Monoidal.Strict         as MMap
import qualified Data.Set.Monad                   as Set
import           GHC.Generics                       (Generic)

import Basis
import Type
import Namespace (Space, PointScope, spaceEntries)


--------------------------------------------------------------------------------
-- * Key types
--

-- | Pipe: component of a computation.  Characterised by:
--   - 'c'  -- either 'Ground' (wire-transportable) or 'Top' (not so)
--   - 'as' -- a type-level list of its argument specs, (TypeSpec (Type k a))
--   - 'o'  -- output type, a single 'TypeSpec' type.
data Pipe (c :: * -> Constraint) (kas :: [*]) (o :: *) (p :: *) where
  Pipe :: PipeConstr c kas o =>
    { pDesc :: Desc c kas o
    , p     :: p
    } -> Pipe c kas o p

pipeName :: (PipeConstr c as o) => Pipe c as o p -> Name Pipe
pipeName (PipeD name _ _ _ _ _ _)   = name
pipeName _ = error "impossible pipeName"

pipeSig :: (PipeConstr c as o) => Pipe c as o p -> Sig
pipeSig   (PipeD _ sig _ _ _ _ _)    = sig
pipeSig _ = error "impossible pipeSig"

pipeStruct :: (PipeConstr c as o) => Pipe c as o p -> Struct
pipeStruct   (PipeD _ _ struct _ _ _ _) = struct
pipeStruct _ = error "impossible pipeStruct"

pipeRep :: (PipeConstr c as o) => Pipe c as o p -> SomeTypeRep
pipeRep   (PipeD _ _ _ rep _ _ _)    = rep
pipeRep _ = error "impossible pipeRep"

showPipe, showPipeP :: Pipe c as o p -> Text
showPipe  Pipe{pDesc} = showDesc  pDesc
showPipeP Pipe{pDesc} = showDescP pDesc

showLR :: Text -> Text -> Text
showLR l r = "left "<>l<>", right "<>r

showLRP :: Pipe c1 as1 o1 Dynamic -> Pipe c2 as2 o2 Dynamic -> Text
showLRP l r = showLR (showPipe l) (showPipe r)


-- | A wrapped cartesian product of all possible kinds of 'Pipe's:
--   - wire-transportable types (constrained 'Ground') vs. 'Top'-(un-)constrained
--   - saturated vs. unsaturated.
data SomePipe (p :: *)
  = forall (kas :: [*]) (o :: *)
    .     (PipeConstr  Ground kas o)
    => G  (Pipe        Ground (kas :: [*]) (o :: *) (p :: *))
  | forall (kas :: [*]) (o :: *)
    .     (PipeConstr  Top    kas o)
    => T  (Pipe        Top    (kas :: [*]) (o :: *) (p :: *))

somePipeName :: SomePipe p -> Name Pipe
somePipeName (GPipeD name _ _ _) = coerceName name
somePipeName (TPipeD name _ _ _) = coerceName name
somePipeName _ = error "impossible somePipeName"

somePipeSig :: SomePipe p -> Sig
somePipeSig  (GPipeD _ sig _ _) = sig
somePipeSig  (TPipeD _ sig _ _) = sig
somePipeSig _ = error "impossible somePipeSig"

somePipeRep :: SomePipe p -> SomeTypeRep
somePipeRep p = withSomePipe p pipeRep

withSomePipe
  :: forall (p :: *) (a :: *)
   . SomePipe p
  -> (forall (c :: * -> Constraint) (kas :: [*]) (o :: *)
      . (PipeConstr c kas o)
      => Pipe c kas  o  p -> a)
  -> a
withSomePipe (G x) = ($ x)
withSomePipe (T x) = ($ x)

-- withSomePipe'
--   :: forall (p :: *)
--   .  SomePipe p
--   -> (forall (c :: * -> Constraint)
--              (kas  :: [*]) (o  :: *)
--              (kas' :: [*]) (o' :: *)
--       . (PipeConstr c kas o)
--       =>  Pipe c kas  o  p
--       -> (Pipe c kas' o' p -> SomePipe p)
--       -> SomePipe p)
--   -> SomePipe p
-- withSomePipe' (G x) f = f x G
-- withSomePipe' (T x) f = f x T
-- withSomePipe'
--   :: forall (p :: *)
--   .  SomePipe p
--   -> (forall (c :: * -> Constraint)
--              (kas  :: [*]) (o  :: *)
--              (kas' :: [*]) (o' :: *)
--       . (PipeConstr c kas o, PipeConstr c kas' o')
--       =>  Pipe c kas  o  p
--       -> (Pipe c kas' o' p -> SomePipe p)
--       -> SomePipe p)
--   -> SomePipe p
-- withSomePipe' (G x) f = f x G
-- withSomePipe' (T x) f = f x T

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

somePipeUncons
  :: forall (p :: *) (e :: *)
  . ()
  => SomePipe p
  -> (forall (c :: * -> Constraint) (o :: *) (kas :: [*])
      . (PipeConstr c kas o, kas ~ '[])
      => Pipe c '[] o p -> e)
  -> (forall (c :: * -> Constraint) (o :: *) (kas :: [*])
             (ka :: *) (kas' :: [*])
      . (PipeConstr c kas o, PipeConstr c kas' o, kas ~ (ka:kas'))
      => Pipe c kas o p -> Either e (Pipe c kas' o p))
  -> Either e (SomePipe p)
somePipeUncons (G p@(Pipe Desc {pdArgs = SOP.Nil   } _)) nil _  = Left $ nil p
somePipeUncons (G p@(Pipe Desc {pdArgs = _ SOP.:* _} _)) _ cons = G <$> cons p
somePipeUncons (T p@(Pipe Desc {pdArgs = SOP.Nil   } _)) nil _  = Left $ nil p
somePipeUncons (T p@(Pipe Desc {pdArgs = _ SOP.:* _} _)) _ cons = T <$> cons p

class    ( Typeable (TagOf ct), Typeable (TypeOf ct), Typeable ct
         , Top ct
         , ct ~ Type (TagOf ct) (TypeOf ct)
         ) => IsType (ct :: *)
instance ( Typeable (TagOf ct), Typeable (TypeOf ct), Typeable ct
         , Top ct
         , ct ~ Type (TagOf ct) (TypeOf ct)
         ) => IsType (ct :: *)

type ArgConstr (c :: * -> Constraint) (ct :: *)
  = ( IsType ct, Typeable c, c (TypeOf ct))

type PipeConstr (c :: * -> Constraint) (kas :: [*]) (o :: *)
  = ( All IsType kas, ArgConstr c o
    , All Typeable kas -- why do we need this, when we have IsType?
    )

-- | Result of running a pipe.
type Result a = IO (Either Text a)

type family PipeFunTy (as :: [*]) (o :: *) :: * where
  PipeFunTy '[]    o = Result (ReprOf o)
  PipeFunTy (x:xs) o = ReprOf x -> PipeFunTy xs o

-- | Everything there is to be said about a pipe, except for its function.
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

descOutTag :: Desc c kas (Type to o) -> Tag to
descOutTag = tpTag . pdOut

descOutType :: Desc c kas (Type to o) -> Proxy o
descOutType = tpType . pdOut

showDesc, showDescP :: Desc c as o -> Text
showDesc  p = pack $ show (pdName p) <>" :: "<>show (pdSig p)
showDescP = ("("<>) . (<>")") . showDesc

-- | Sig:  serialisable type signature
data Sig =
  Sig
  { sArgs :: [SomeType]
  , sOut  :: SomeType
  }
  deriving (Eq, Generic, Ord)

showSig :: Sig -> Text -- " ↦ ↣ → ⇨ ⇒ "
showSig (Sig as o) = T.intercalate " ⇨ " $ showSomeType <$> (as <> [o])

-- | Struct: Pipe's internal structure, as a graph of type transformations.
newtype Struct = Struct (G.Graph SomeType) deriving (Eq, Generic, Ord, Show)

--------------------------------------------------------------------------------
-- * PipeSpace -- move to Pipe.Space?
--
type SomePipeSpace p = PipeSpace (SomePipe p)

data PipeSpace a = PipeSpace
  { psName  :: !(QName PipeSpace)
  , psSpace :: !(Space Point a)
  , psFrom  :: !(MonoidalMap SomeTypeRep (Set (QName Pipe)))
  , psTo    :: !(MonoidalMap SomeTypeRep (Set (QName Pipe)))
  } deriving (Eq, Ord)

instance (Ord a, Serialise a, Typeable a) => Serialise (PipeSpace a) where
  encode PipeSpace{psName, psSpace, psFrom, psTo} =
    encodeListLen 5
    <> encodeWord 2177
    <> encode psName
    <> encode psSpace
    <> encode (MMap.map setToList psFrom)
    <> encode (MMap.map setToList psTo)
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (5, 2177) ->
        PipeSpace
        <$> decode
        <*> decode
        <*> (MMap.map Set.fromList <$> decode)
        <*> (MMap.map Set.fromList <$> decode)
      _ -> fail $ "PipeSpace failed decode: "
           <>" len="<> show len
           <>" tag="<> show tag
           <>" rep="<> show (typeRep @(PipeSpace a))

instance Semigroup (PipeSpace a) where
  l <> r = PipeSpace
    { psName  = psName  l <> psName  r
    , psSpace = psSpace l <> psSpace r
    , psFrom  = psFrom  l <> psFrom  r
    , psTo    = psTo    l <> psTo    r
    }

type SomePipeScope p = PointScope (SomePipe p)

--------------------------------------------------------------------------------
-- * Operations
--
data Ops p where
  Ops ::
    { app
      :: forall c kas kas' o ka
      . ( PipeConstr c kas  o
        , PipeConstr c kas' o
        , kas ~ (ka : kas')
        )
      => Desc c kas o -> Value (TagOf ka) (TypeOf ka) -> p -> Either Text p
    , comp
      :: forall cf cv vas vo fas fass ras fo
      . ( PipeConstr cv vas vo
        , PipeConstr cf fas fo
        , fas ~ (vo:fass)
        , ras ~ fass
        )
      => Desc cv vas vo -> p -> Desc cf fas fo -> p -> Either Text p
    , trav
      :: forall cf ct fas fo a tas to
      . ( PipeConstr cf fas fo
        , PipeConstr ct tas to
        , fas ~ (Type Point a ': '[])
        , tas ~ '[]
        , TypeOf to ~ a
        , TagOf fo ~ 'Point)
      => Desc cf fas fo -> p -> Desc ct tas to -> p -> Either Text p
    } -> Ops p

-- This allows pipe operations (apply, compose, traverse) to be
-- performed over the HKDT.
class PipeOps p where
  pipeOps   :: Ops p

--------------------------------------------------------------------------------
-- * Important instances
--
instance Eq (Pipe c as o ()) where
  -- XXX: potentially problematic instance
  (==) = (==) `on` pDesc

instance Ord (Pipe c as o ()) where
  -- XXX: potentially problematic instance
  compare = compare `on` pDesc

instance Functor (Pipe c as o) where
  fmap f (Pipe d x) = Pipe d (f x)

instance Eq (SomePipe ()) where
  -- XXX: potentially problematic instance
  l' == r' =
    withSomePipe l' $ \(pDesc -> l) ->
    withSomePipe r' $ \(pDesc -> r) ->
      (pdRep    l  == pdRep      r) &&
      (pdName   l  == pdName     r) &&
      (pdStruct l  == pdStruct   r)

instance Ord (SomePipe ()) where
  -- XXX: potentially problematic instance
  l' `compare` r' =
    withSomePipe l' $ \(pDesc -> l) ->
    withSomePipe r' $ \(pDesc -> r) ->
      pdRep l `compare` pdRep    r

instance Functor SomePipe where
  fmap f (G x) = G (f <$> x)
  fmap f (T x) = T (f <$> x)

instance Functor PipeSpace where
  fmap f ps@PipeSpace{psSpace} =
    ps { psSpace = f <$> psSpace }

--------------------------------------------------------------------------------
-- * SomePipe
--
instance Read (SomePipe ()) where
  readPrec = failRead

pattern PipeD ::
                 ( ArgConstr c o
                 , All Typeable kas
                 , All IsType kas
                 , All Top (kas :: [*])
                 )
                 -- (PipeConstr c as o)
              => Name Pipe -> Sig -> Struct -> SomeTypeRep
              -> NP TypePair kas
              -> TypePair o
              -> p
              -> Pipe c kas o p
-- TODO:  get rid of this, the added benefit is too small.
pattern PipeD name sig str rep args out p
              <- Pipe (Desc name sig str rep args out) p

pattern GPipeD, TPipeD :: Name Pipe -> Sig -> Struct -> SomeTypeRep -> SomePipe p
pattern GPipeD name sig str rep <- G (PipeD name sig str rep _ _ _)
pattern TPipeD name sig str rep <- T (PipeD name sig str rep _ _ _)


-- * Desc
--
instance Eq (Desc c as o) where
  -- XXX: slightly opportustic, due to omissions.
  Desc ln _ lst lrep _ _ == Desc rn _ rst rrep _ _ =
    ln == rn && lst == rst && lrep == rrep

instance Ord (Desc c as o) where
  Desc ln _ lst lrep _ _ `compare` Desc rn _ rst rrep _ _ =
    ln `compare` rn <> lrep `compare` rrep <> lst `compare` rst

instance NFData (Name a)
instance NFData (QName a)
instance NFData SomeType
instance NFData Sig
instance NFData (Desc c as o) where
  rnf (Desc x y z w _ u) = -- XXX: ugh
    rnf x `seq` rnf y `seq` rnf z `seq` rnf w `seq` rnf u
instance NFData Struct

{-------------------------------------------------------------------------------
  Really boring.
-------------------------------------------------------------------------------}
instance Typeable a => Read (PipeSpace a) where readPrec = failRead

instance Show (PipeSpace a) where
  show PipeSpace{psName, psSpace} =
    "(PipeSpace "<>show psName<>" "<>show (length $ spaceEntries psSpace)<>" entries)"

instance Show (SomePipe p) where
  show (G p) = "GPipe "<>unpack (showPipe p)
  show (T p) = "TPipe "<>unpack (showPipe p)

instance Show (Pipe c as o p) where
  show p = "Pipe "<>unpack (showPipe p)

instance Show (Desc c as o) where show = unpack . showDesc

instance Show Sig where
  show x  =  "("<>T.unpack (showSig x)<>")"
instance Serialise Sig
instance Read Sig where readPrec = failRead

instance Serialise Struct
instance Read Struct where readPrec = failRead

instance (Typeable c, Typeable as, Typeable o) => Read (Desc c as o) where readPrec = failRead

--- A useless function : -/
-- mapSomePipeEither
--   :: forall (e :: *) (p :: *) -- (kas' :: [*])
--   .  --(All Typeable kas', All PairTypeable kas')
--      ()
--   => SomePipe p
--   -> (forall (c :: * -> Constraint) (kas :: [*]) (kas' :: [*]) (o :: *)
--       . (PipeConstr c kas o, PipeConstr c kas' o)
--       => (Proxy c -> NP (SOP.K ()) kas -> Proxy o -> NP (SOP.K ()) kas')
--       -> Pipe c kas o p
--       -> Either e (Pipe c kas' o p))
--   -> Either e (SomePipe p)
-- mapSomePipeEither (G x) f = G <$> f tf x
--   where
--     tf :: Proxy c -> NP (SOP.K ()) kas -> Proxy o -> NP (SOP.K ()) kas'
--     tf c (_ SOP.:* xs) o = xs
-- mapSomePipeEither (T x) f = T <$> f Proxy x
