module Dom.SomePipe (module Dom.SomePipe,
                     module Dom.SomePipe.SomePipe)
where

import Basis

import Dom.CTag
import Dom.Cap
import Dom.Ground
import Dom.LTag
import Dom.Name
import Dom.Pipe
import Dom.Pipe.Constr
import Dom.Pipe.IOA
import Dom.Result
import Dom.Sig
import Dom.SomePipe.SomePipe
import Dom.SomeValue
import Dom.SomeVTag
import Dom.Struct
import Dom.Tags
import Dom.VTag



-- * Running
--
runSomePipe :: HasCallStack => SomePipe Dynamic -> SomeResult --Result l SomeValue
runSomePipe SP{..} = go spPipe spCaps
 where
   go :: PipeConstr l cas o
      => Pipe l cas o Dynamic -> Caps (CTagVV o) -> SomeResult
   go Pipe{pDesc=Desc{pdLTag=LNow, pdOut=Tags{tCTag, tVTag}}
          ,p} caps =
     SR LNow $ (fmap (SV tCTag . SVK tVTag caps)
                <$> runIOADynamic p LNow tCTag tVTag
                :: Result Now SomeValue)
   go Pipe{pDesc=Desc{pdLTag=ltag@LLive{}, pdOut=Tags{tCTag, tVTag}}
          ,p} caps =
     SR ltag $ (fmap (fmap (SV tCTag . SVK tVTag caps))
                <$> runIOADynamic p ltag tCTag tVTag)

recoverPipe ::
     QName Pipe
  -> Name Pipe
  -> ISig
  -> Struct
  -> SomeTypeRep
  -> SomeLTag
  -> [(SomeCTag, SomeVTag, SomeTypeRep, SomeTypeRep)]
     -- ^ NOTE: 'out' first, then 'args'
  -> Either String (SomePipe ())
recoverPipe _  _    _   _      _   _ [] =
  Left "Empty list for (out:args)."
recoverPipe qn name sig struct rep (SomeLTag ltag) (out:args) =
  Right $ somePipeSetQName qn $
    withRecoveredTags out $
      -- Start with a saturated pipe, and then build it up with arguments.
      \outTags _pc _pa -> go args $
        mkPipeBase ltag outTags name sig struct rep
 where
   go :: [(SomeCTag, SomeVTag, SomeTypeRep, SomeTypeRep)]
      -> SomePipe () -> SomePipe ()
   go [] sp = sp
   go (x:xs) (SP n caps (Pipe (Desc{..} :: Desc l as o) _)) =
     withRecoveredTags x $
       \(tip :: Tags a)
        (_ :: Proxy (CTagVC a)) (_ :: Proxy (CTagVV a))
       -> go xs $ SP n caps $ Pipe
          (Desc pdName pdSig pdStruct pdRep pdLTag (tip :* pdArgs) pdOut
           :: Desc l (a:as) o) ()

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
  :: forall (l :: Liveness) out. (ArgConstr out, LiveConstr l, PipeConstr l '[] out)
  => LTag l -> Tags out -> Name Pipe -> ISig -> Struct -> SomeTypeRep -> SomePipe ()
mkPipeBase ltag out name sig struct rep =
  case lookupGroundByRep (SomeTypeRep outRep) of
    Nothing ->
      -- Non-ground (unknown) type, nothing useful we can recapture about it.
      -- trace ("no Ground for:  " <> show name <> "/" <> show outRep) $
      withTypeable outRep $
        SP mempty capsT $
          Pipe (Desc name sig struct rep ltag Nil out) ()
    Just (_, _, _, TyDict (_ :: Ground b' => Proxy b')) ->
      case typeRep @b' `eqTypeRep` typeRep @(CTagVV out) of
        Just HRefl ->
          -- trace ("Ground for:  " <> show name <> "/" <> show outRep) $
          SP mempty capsTSG $
            Pipe (Desc name sig struct rep ltag Nil out :: Desc l '[] out)
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

somePipeSomeLTag :: SomePipe p -> SomeLTag
somePipeSomeLTag p = withSomePipe p $
  \Pipe{pDesc=Desc{pdLTag}} ->
    SomeLTag pdLTag

somePipeHasCap :: Cap c -> SomePipe p -> Bool
somePipeHasCap c SP{..} = hasCap c spCaps
