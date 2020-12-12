module Dom.Pipe.SomePipe (module Dom.Pipe.SomePipe) where

import Data.Dynamic                     qualified as Dynamic
import Data.Reflection
import Data.Void
import Generics.SOP                     qualified as SOP
import Text.Read                                    (ReadPrec)
import Unsafe.Coerce                    qualified as Unsafe

import Basis

-- import Data.Class
import Data.Orphanage
import Data.Parsing

import Dom.CTag
import Dom.Cap
import Dom.Error
import Dom.Ground
import Dom.Name
import Dom.Parse
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
  = forall (kas :: [*]) (o :: *). (PipeConstr   kas o) =>
    SP
    { spQName :: !(QName Pipe)
    , spCaps  :: !(Caps (CTagVV o))
    , spPipe  :: !(Pipe (kas :: [*]) (o :: *) (p :: *))
    }

pattern SPipeD :: Name Pipe -> ISig -> Struct -> SomeTypeRep -> SomePipe p
pattern SPipeD name sig str rep <- SP _ caps (PipeD name sig str rep _ _ _)

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
  fmap f (SP h c x) = SP h c (f <$> x)

instance Foldable SomePipe where
  foldMap toM = \case
    SP _ _ (Pipe _ x) -> toM x

instance Traversable SomePipe where
  traverse f = \case
    SP n c (Pipe d x) -> f x <&> \x' -> SP n c (Pipe d x')

instance Show (SomePipe p) where
  show (SP _ _ p) = "SomePipe "<>unpack (showPipe p)


-- * Running
--
runSomePipe :: HasCallStack => SomePipe Dynamic -> Result SomeValue
runSomePipe SP{spPipe=Pipe{pDesc=pd@Desc{pdOut=Tags{tCTag, tVTag}} :: Desc as o
                        ,p}, ..} =
  case Dynamic.fromDynamic p :: Maybe (IOA '[] o) of
    Nothing -> fallM $ "GPipe isn't a runnable Ground IOA: " <> showDesc pd
    Just (IOA io _as _o) ->
      (SV tCTag . SVK tVTag spCaps . mkValue tCTag tVTag <$>) <$> io

newtype Magic c r = Magic (forall a. c a => Proxy a -> r)
reifyConstr' :: forall r b c. Proxy c -> b -> (forall a. c a => Proxy a -> r) -> r
reifyConstr' _p n k = Unsafe.unsafeCoerce
                       (Magic k :: Magic c r)
                       n
                       Proxy
{-# INLINE reifyConstr' #-}

recoverPipe ::
     QName Pipe
  -> Name Pipe
  -> ISig
  -> Struct
  -> SomeTypeRep
  -> [(SomeCTag, SomeVTag, SomeTypeRep, SomeTypeRep)]
     -- ^ NOTE: 'out' first, then 'args'
  -> Either String (SomePipe ())
recoverPipe _  _    _   _      _   [] =
  Left "Empty list for (out:args)."
recoverPipe qn name sig struct rep (out:args) =
  Right $ somePipeSetQName qn $
    withRecoveredTags out $
      -- Start with a saturated pipe, and then build it up with arguments.
      \outTags _pc _pa -> go args $
         mkPipeBase outTags name sig struct rep
 where
   go :: [(SomeCTag, SomeVTag, SomeTypeRep, SomeTypeRep)]
      -> SomePipe () -> SomePipe ()
   go [] sp = sp
   go (x:xs) (SP n caps (Pipe (Desc{..} :: Desc cas o) _)) =
     withRecoveredTags x $
       \(tip :: Tags ca)
        (_ :: Proxy (CTagVC ca)) (_ :: Proxy (CTagVV ca))
       -> go xs $ SP n caps $ Pipe
          (Desc pdName pdSig pdStruct pdRep (tip :* pdArgs) pdOut
           :: Desc (ca:cas) o) ()

   withRecoveredTags ::
     forall b
     . (SomeCTag, SomeVTag, SomeTypeRep, SomeTypeRep)
     -> (forall (c1 :: Con) (a1 :: *) ty
         . ( Typeable (CTagV c1 a1), Typeable c1, Typeable a1
           , ReifyCTag c1, ReifyVTag a1
           , ty ~ CTagV c1 a1)
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
           f (Tags tc tv :: Tags (CTagV c a))
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
  :: forall out. (ArgConstr out)
  => Tags out -> Name Pipe -> ISig -> Struct -> SomeTypeRep -> SomePipe ()
mkPipeBase out name sig struct rep =
  case lookupGroundByRep (SomeTypeRep outRep) of
    Nothing ->
      -- Non-ground (unknown) type, nothing useful we can recapture about it.
      -- trace ("no Ground for:  " <> show name <> "/" <> show outRep) $
      withTypeable outRep $
        SP mempty capsT $
          Pipe (Desc name sig struct rep Nil out) ()
    Just (_, _, _, TyDict (_ :: Ground b' => Proxy b')) ->
      case typeRep @b' `eqTypeRep` typeRep @(CTagVV out) of
        Just HRefl ->
          -- trace ("Ground for:  " <> show name <> "/" <> show outRep) $
          SP mempty capsTSG $
            Pipe (Desc name sig struct rep Nil out :: Desc '[] out)
                 ()
        Nothing -> error $
          "mkSaturatedPipe:  internal inconsistency while looking up Dict for type "
          <> unpack (showName name)
 where outRep = typeRep @(CTagVV out)

--------------------------------------------------------------------------------
-- * Utils
--
somePipeSetQName :: QName Pipe -> SomePipe p -> SomePipe p
somePipeSetQName x (SP _ c p) = SP x c p { pDesc = (pDesc p) { pdName = lastQName x } }

somePipeQName :: SomePipe p -> QName Pipe
somePipeQName = spQName

somePipeName :: SomePipe p -> Name Pipe
somePipeName (SPipeD name _ _ _) = coerceName name
somePipeName _ = error "impossible somePipeName"

somePipeSig :: SomePipe p -> ISig
somePipeSig  (SPipeD _ sig _ _) = sig
somePipeSig _ = error "impossible somePipeSig"

somePipeRep :: SomePipe p -> SomeTypeRep
somePipeRep p = withSomePipe p pipeRep

somePipeHasCap :: Cap c -> SomePipe p -> Bool
somePipeHasCap c SP{..} = hasCap c spCaps

withSomePipe
  :: forall (p :: *) (a :: *)
   . SomePipe p
  -> (forall (kas :: [*]) (o :: *)
      . (PipeConstr kas o)
      => Pipe kas  o  p -> a)
  -> a
withSomePipe SP{..} = ($ spPipe)

somePipeUncons
  :: forall (p :: *) (e :: *)
  . ()
  => SomePipe p
  -> (forall (o :: *) (kas :: [*])
      . (PipeConstr kas o, kas ~ '[])
      => Pipe '[] o p -> e)
  -> (forall (o :: *)
             (ka :: *) (kas' :: [*])
      . (PipeConstr (ka:kas') o, PipeConstr kas' o)
      => Pipe (ka:kas') o p -> Either e (Pipe kas' o p))
  -> Either e (SomePipe p)
somePipeUncons (SP _ _ p@(Pipe Desc {pdArgs = Nil   } _)) nil _  = Left $ nil p
somePipeUncons (SP n c p@(Pipe Desc {pdArgs = _ :* _} _)) _ cons = SP n c <$> cons p

somePipeOutSomeCTagType :: SomePipe p -> (SomeCTag, SomeTypeRep)
somePipeOutSomeCTagType p =
  withSomePipe p pipeOutSomeCTagType

somePipeTraverse
  :: SomePipe p
  -> SomePipe p
  -> (forall (fa :: *) (fo :: *) (to :: *)
      . ( PipeConstr (fa:'[]) fo
        , PipeConstr '[]      to)
      => Pipe (fa:'[]) fo p
      -> Pipe '[] to p
      -> Fallible (Pipe '[] (CTagV (CTagVC to) (CTagVV fo)) p))
  -> Fallible (SomePipe p)
somePipeTraverse (SP _ sf f@(Pipe Desc{pdArgs=_:* Nil} _)) (SP _ _ t@(Pipe Desc{pdArgs=Nil} _)) trav = SP mempty sf <$> trav f t
somePipeTraverse f t _ = Left $
  if | fA == 0 -> "Supposed function is saturated."
     | fA  > 1 -> "Non-singular function, arity: "   <> showT fA & Error
     | tA  > 0 -> "Unsaturated traversable, arity: " <> showT tA & Error
     | True    -> "Unknown error."
 where fA = somePipeArity f
       tA = somePipeArity t

somePipeArity :: SomePipe p -> Int
somePipeArity sp = withSomePipe sp $
  \p -> length (SOP.hcollapse . SOP.hmap (K . const ()) . pdArgs $ pDesc p)

somePipeArityCase
  :: forall (p :: *) (a :: *)
  .  SomePipe p
  -- Zero.
  -> (forall o (cas :: [*])
      . (PipeConstr cas o, cas ~ '[])
      => Pipe cas o p -> a)
  -- One.
  -> (forall o (cas :: [*]) (ca :: *)
      . (PipeConstr cas o, cas ~ (ca:'[]),  PipeConstr '[] o)
      => Pipe cas o p -> a)
  -- Infinity.
  -> (forall o (cas :: [*]) (ca :: *) (cas' :: [*])
      . (PipeConstr cas o, cas ~ (ca:cas'), PipeConstr cas' o)
      => Pipe cas o p -> a)
  -> a
somePipeArityCase (SP _ _ p@(Pipe Desc {pdArgs =      Nil} _)) z _ _ = z p
somePipeArityCase (SP _ _ p@(Pipe Desc {pdArgs = _ :* Nil} _)) _ s _ = s p
somePipeArityCase (SP _ _ p@(Pipe Desc {pdArgs = _ :* _}   _)) _ _ n = n p
