module Dom.Pipe.SomePipe (module Dom.Pipe.SomePipe) where

import qualified Data.Dynamic                     as Dynamic

import Basis

import Data.Orphanage

import Dom.CTag
import Dom.Error
import Dom.Ground
import Dom.Name
import Dom.Pipe
import Dom.Pipe.Constr
import Dom.Pipe.IOA
import Dom.Sig
import Dom.SomeValue
import Dom.SomeVTag
import Dom.Struct
import Dom.Tags
import Dom.Value
import Dom.VTag


--------------------------------------------------------------------------------
-- * Key types
--
-- | A wrapped cartesian product of all possible kinds of 'Pipe's:
--   - wire-transportable types (constrained 'Ground') vs. 'Top'-(un-)constrained
--   - saturated vs. unsaturated.
data SomePipe (p :: *)
  = forall (kas :: [*]) (o :: *). (PipeConstr  Ground kas o) =>
    G
    { spQName :: !(QName Pipe)
    , gPipe   :: !(Pipe Ground (kas :: [*]) (o :: *) (p :: *))
    }
  | forall (kas :: [*]) (o :: *). (PipeConstr  Top    kas o) =>
    T
    { spQName :: !(QName Pipe)
    , tPipe   :: !(Pipe Top    (kas :: [*]) (o :: *) (p :: *))
    }

pattern GPipeD, TPipeD :: Name Pipe -> ISig -> Struct -> SomeTypeRep -> SomePipe p
pattern GPipeD name sig str rep <- G _ (PipeD name sig str rep _ _ _)
pattern TPipeD name sig str rep <- T _ (PipeD name sig str rep _ _ _)

--------------------------------------------------------------------------------
-- * Instances
--
-- XXX: We risk equating different pipes with same names and types.
instance Eq (SomePipe p) where
  l' == r' =
    withSomePipe l' $ \(pDesc -> l) ->
    withSomePipe r' $ \(pDesc -> r) ->
      (pdRep    l  == pdRep      r) &&
      (pdName   l  == pdName     r) &&
      (pdStruct l  == pdStruct   r)

-- XXX: We risk equating different pipes with same names and types.
instance Ord (SomePipe p) where
  l' `compare` r' =
    withSomePipe l' $ \(pDesc -> l) ->
    withSomePipe r' $ \(pDesc -> r) ->
      pdRep l `compare` pdRep    r

instance Read (SomePipe ()) where
  readPrec = failRead

instance Functor SomePipe where
  fmap f (G h x) = G h (f <$> x)
  fmap f (T h x) = T h (f <$> x)

instance Show (SomePipe p) where
  show (G _ p) = "GPipe "<>unpack (showPipe p)
  show (T _ p) = "TPipe "<>unpack (showPipe p)


-- * Running
--
runSomePipe :: SomePipe Dynamic -> Result SomeValue
runSomePipe T{tPipe=t@Pipe{}} = fallM $ "runPipe:  non-Ground pipe: " <> showPipe t
runSomePipe G{gPipe=  Pipe{pDesc, p}} = _runPipe pDesc p

somePipeRunnabilityIssues :: SomePipe a -> Maybe Error
somePipeRunnabilityIssues = \case
  T{} -> Just "Not a ground pipe"
  G{gPipe=Pipe{pDesc=Desc{pdArgs=_:*_}}} -> Just "Not a saturated pipe"
  G{gPipe=Pipe{pDesc=Desc{pdArgs=Nil}}} -> Nothing

_runPipe
  :: forall c (as :: [*]) o. PipeConstr c as o
  => Desc c as o
  -> Dynamic
  -> Result SomeValue
_runPipe pd@Desc{pdOut=Tags{tCTag, tVTag}} dyn =
  case Dynamic.fromDynamic dyn :: Maybe (IOA Ground '[] o) of
    Nothing -> fallM $ "Incomplete pipe: " <> showDesc pd
    Just (IOA io _c _as _o) ->
      (SomeValue tCTag . SomeValueKinded tVTag . mkValue tCTag tVTag <$>) <$> io


recoverPipe ::
     QName Pipe
  -> Name Pipe
  -> ISig
  -> Struct
  -> SomeTypeRep
  -> [(SomeCTag, SomeVTag, SomeTypeRep, SomeTypeRep)]
  -> SomePipe ()
recoverPipe qname name sig struct rep xs =
  somePipeSetQName qname $ withRecoveredTags (head xs) $
  -- Start with a saturated pipe, and then build it up with arguments.
  \out _ _ -> go xs $
    mkPipeBase (Proxy @Top) out name sig struct rep
 where
   go :: [(SomeCTag, SomeVTag, SomeTypeRep, SomeTypeRep)]
      -> SomePipe () -> SomePipe ()
   go []     p = p
   go xs p = withSomePipeGroundCase p (goGround xs) (goTop xs)

   goGround ::
        [(SomeCTag, SomeVTag, SomeTypeRep, SomeTypeRep)]
     -> Pipe Ground cas o ()
     -> SomePipe ()
   goGround (x:xs) (Pipe (Desc{..} :: Desc Ground cas o) _) =
     withRecoveredTags x $
       \(tip :: Tags ca)
        (_ :: Proxy (TypesC ca)) (_ :: Proxy (TypesV ca))
       -> go xs $ G mempty $ Pipe
          (Desc pdName pdSig pdStruct pdRep (tip :* pdArgs) pdOut
           :: Desc Ground (ca:cas) o) ()

   goTop ::
        [(SomeCTag, SomeVTag, SomeTypeRep, SomeTypeRep)]
     -> Pipe Top cas o ()
     -> SomePipe ()
   goTop (x:xs) (Pipe (Desc{..} :: Desc Top cas o) _) =
     withRecoveredTags x $
       \(tip :: Tags ca)
        (_ :: Proxy (TypesC ca)) (_ :: Proxy (TypesV ca))
       -> go xs $ T mempty $ Pipe
          (Desc pdName pdSig pdStruct pdRep (tip :* pdArgs) pdOut
           :: Desc Top (ca:cas) o) ()

   withRecoveredTags
     :: forall b
     . (SomeCTag, SomeVTag, SomeTypeRep, SomeTypeRep)
     -> (forall (c1 :: Con) (a1 :: *) ty
         . ( Typeable (Types c1 a1), Typeable c1, Typeable a1
           , ReifyCTag c1, ReifyVTag a1
           , ty ~ Types c1 a1)
         => Tags ty -> Proxy c1 -> Proxy a1 -> b)
     -> b
   withRecoveredTags
     ( SomeCTag    (tc :: CTag    c),  SomeVTag    (tv :: VTag    a)
     , SomeTypeRep (rc :: TypeRep cr), SomeTypeRep (rv :: TypeRep ar)
     ) f =
     case (rc `eqTypeRep` typeRep @c,  rv `eqTypeRep` typeRep @a)
     of
       (Just HRefl, Just HRefl) ->
         withTypeable rc $ withTypeable rv $ withCTag tc $
           f (Tags tc tv :: Tags (Types c a))
             (Proxy @c) (Proxy @a)
       (,) Nothing _ -> error $ mconcat
         [ "withRecoveredTags: container tag miss: Ctag c=", show $ typeRep @c
         , " rc=", show rc
         ]
       (,) _ Nothing -> error $ mconcat
         [ "withRecoveredTags: value tag miss: VTag a=", show $ typeRep @a
         , " rv=", show rv
         ]

-- | Use the ground type table to reconstruct a saturated,
--   and possibly Ground-ed SomePipe.
mkPipeBase
  :: forall c out. (ArgConstr c out)
  => Proxy c -> Tags out -> Name Pipe -> ISig -> Struct -> SomeTypeRep -> SomePipe ()
mkPipeBase _c out name sig struct rep =
  case lookupGroundByRep outRep of
    Nothing ->
      -- Non-ground (unknown) type, nothing useful we can recapture about it.
      trace ("no Ground for:  " <> show name <> "/" <> show outRep) $
      T mempty $
      Pipe (Desc name sig struct rep Nil out :: Desc Top '[] out) ()
    Just (_, _, _, TyDict (_ :: Ground b' => Proxy b')) ->
      case typeRep @b' `eqTypeRep` typeRep @(TypesV out) of
        Just HRefl ->
          trace ("Ground for:  " <> show name <> "/" <> show outRep) $
          G mempty
          (Pipe (Desc name sig struct rep Nil out :: Desc Ground '[] out)
                () :: Pipe Ground '[] out ())
        Nothing -> error $
          "mkSaturatedPipe:  internal inconsistency while looking up Dict for type "
          <> unpack (showName name)
 where outRep = someTypeRep $ Proxy @(TypesV out)

--------------------------------------------------------------------------------
-- * Utils
--
somePipeSetQName :: QName Pipe -> SomePipe p -> SomePipe p
somePipeSetQName x (G _ p) = G x p
somePipeSetQName x (T _ p) = T x p

somePipeQName :: SomePipe p -> QName Pipe
somePipeQName = spQName

somePipeName :: SomePipe p -> Name Pipe
somePipeName (GPipeD name _ _ _) = coerceName name
somePipeName (TPipeD name _ _ _) = coerceName name
somePipeName _ = error "impossible somePipeName"

somePipeSig :: SomePipe p -> ISig
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
withSomePipe G{..} = ($ gPipe)
withSomePipe T{..} = ($ tPipe)

withSomeGroundPipe
  :: forall (p :: *) (a :: *)
   . SomePipe p
  -> (forall (kas :: [*]) (o :: *)
      . (PipeConstr Ground kas o)
      => Pipe Ground kas  o  p -> a)
  -> Maybe a
withSomeGroundPipe G{..} f = Just (f gPipe)
withSomeGroundPipe T{..} _ = Nothing

withSomePipeGroundCase
  :: forall (p :: *) (a :: *)
   . SomePipe p
  -> (forall (kas :: [*]) (o :: *)
      . (PipeConstr Ground kas o)
      => Pipe Ground kas  o  p -> a)
  -> (forall (kas :: [*]) (o :: *)
      . (PipeConstr Top    kas o)
      => Pipe Top    kas  o  p -> a)
  -> a
withSomePipeGroundCase G{..} g _ = g gPipe
withSomePipeGroundCase T{..} _ t = t tPipe

somePipeUncons
  :: forall (p :: *) (e :: *)
  . ()
  => SomePipe p
  -> (forall (c :: * -> Constraint) (o :: *) (kas :: [*])
      . (PipeConstr c kas o, kas ~ '[])
      => Pipe c '[] o p -> e)
  -> (forall (c :: * -> Constraint) (o :: *)
             (ka :: *) (kas' :: [*])
      . (PipeConstr c (ka:kas') o, PipeConstr c kas' o)
      => Pipe c (ka:kas') o p -> Either e (Pipe c kas' o p))
  -> Either e (SomePipe p)
somePipeUncons (G _ p@(Pipe Desc {pdArgs = Nil   } _)) nil _  = Left $ nil p
somePipeUncons (G h p@(Pipe Desc {pdArgs = _ :* _} _)) _ cons = G h <$> cons p
somePipeUncons (T _ p@(Pipe Desc {pdArgs = Nil   } _)) nil _  = Left $ nil p
somePipeUncons (T h p@(Pipe Desc {pdArgs = _ :* _} _)) _ cons = T h <$> cons p

somePipeOutSomeCTagType :: SomePipe p -> (SomeCTag, SomeTypeRep)
somePipeOutSomeCTagType p =
  withSomePipe p pipeOutSomeCTagType
