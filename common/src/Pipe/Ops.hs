{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeInType                 #-}

module Pipe.Ops
  (
  -- * High-level
    compile
  , assemble
  -- * Ops
  , runPipe
  , apply
  , compose
  -- * Constructors
  , gen
  , link
  )
where

import qualified Algebra.Graph                    as G
import           Data.Dynamic                       (fromDynamic)
import qualified Data.Kind                        as K
import           Type.Reflection

import qualified Unsafe.Coerce                    as Unsafe

import Basis
import Pipe.Expr
import Pipe.Types
import Type


-- * Compiling pipe expressions
--
compile
  :: e ~ Text
  => (QName Pipe -> Either Text Pipe)
  -> Text
  -> Either e Pipe
compile lookupPipe = join . (assemble <$>) . parse lookupPipe


-- * The actual type, private.
--
data PipeFun (ka :: Con) a (kb :: Con) b where
  FOutput :: forall kb b
           . (Typeable kb, Ground b)
          => Tag kb b ->               Result (Repr kb b)  -> PipeFun 'Point () kb b
  FLink   :: forall ka a kb b
           . (Typeable ka, Ground a, Typeable kb, Ground b)
          => Tag kb b -> (Repr ka a -> Result (Repr kb b)) -> PipeFun    ka  a kb b

pattern App4 :: TyCon -> TypeRep a -> TypeRep ka -> TypeRep b -> TypeRep kb -> TypeRep c
pattern App4 con a ka b kb <- App (App (App (App (Con con) ka) a) kb) b

pattern DApp4 :: c -> TypeRep c -> TyCon -> TypeRep a -> TypeRep ka -> TypeRep b -> TypeRep kb -> Dynamic
pattern DApp4 v tr con a ka b kb <- Dynamic tr@(App4 con ka a kb b) v

assemble
  :: e ~ Text
  => Expr
  -> Either e Pipe
assemble = go
  where
    go :: Expr -> Either Text Pipe
    go (PPipe p) = Right p

    go (PApp    PVal{}  _)          = Left $ "Applying a value."
    go (PApp  f           PVal{vX}) = join $ apply <$> go f <*> pure [vX]
    -- here we expect traverseP to reduce:
    --   PApp (b -> IO c) (a -> IO b) -> (a -> IO c)
    go (PApp  f@PPipe{} x@PPipe{})  =        traverseP          (pP f)    (pP x)
    go (PApp  f@PPipe{} x@PComp{})  = join $ traverseP <$> pure (pP f) <*> go x
    go (PApp  f         x)          = join $ traverseP <$> go f        <*> go x

    go (PComp   PVal{}  _)          = Left $ "Composing a value on the left."
    go (PComp _           PVal{})   = Left $ "Composing a value on the right."
    go (PComp l@PPipe{} r@PPipe{})  = compose (pP l) (pP r)
    go (PComp l         r)          = join $ compose <$> (go l) <*> (go r)

    go PVal{} = Left "Processing a value should never happen."


-- * Constructors
--
gen
  :: forall kt tt
  .  (Typeable (Repr kt tt), Typeable kt, Ground tt)
  => Name Pipe
  -> Tag kt tt
  -> Result (Repr kt tt)
  -> Pipe
gen name tagTo mv
                = Pipe name sig struct tagTo dyn
  where ty      = tagType tagTo
        sig     = Gen ty
        struct  = Struct graph
        graph   = G.vertex ty
        dyn     = Dynamic typeRep pipeFun
        pipeFun = FOutput tagTo mv :: PipeFun Point () kt tt

link
  :: forall kf tf kt tt
  . ( Typeable (Repr kf tf)
    , Typeable (Repr kt tt)
    , Typeable kf
    , Ground tf
    , Typeable kt
    , Ground tt)
  => Name Pipe
  -> Tag kf tf
  -> Tag kt tt
  -> (Repr kf tf -> Result (Repr kt tt))
  -> Pipe
link name tagFrom tagTo mf
                = Pipe name sig struct tagTo dyn
  where tyFrom  = tagType tagFrom
        tyTo    = tagType tagTo
        sig     = Link tyFrom tyTo
        struct  = Struct graph
        graph   = G.connect (G.vertex tyFrom) (G.vertex tyTo)
        dyn     = Dynamic typeRep pipeFun
        pipeFun = FLink tagTo mf :: PipeFun kf tf kt tt


-- * Running
--
runPipe :: Pipe -> Result SomeValue
runPipe (Pipe _ _ _ tag dyn) = runPipe' tag dyn

runPipe' :: forall k a
          . (Typeable k, Ground a)
         => Tag (k :: Con) (a :: *)
         -> Dynamic
         -> Result SomeValue
runPipe' _tagTo dyn =
  case fromDynamic dyn :: Maybe (PipeFun Point () k a) of
    Nothing -> pure . Left $ "Incomplete pipe: " <> pack ""
    Just (FOutput tagTo pipeFun) -> do
      (SomeValue . SomeKindValue . mkValue tagTo <$>) <$> pipeFun



-- * Basic ops:  unwrapping Dynamics
--
-- | 'traverseP': approximate 'traverse':
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
traverseP :: Pipe -> Pipe -> Either Text Pipe
traverseP p@PGen{} _
  =   Left $ "'traverse': left fully saturated: " <> showPipe p
traverseP
  l@(PLink lpn ls (DApp4 ml _lrep ltc         kpb b' _kc _c) lfrom _lto)
  r@(PAny  rpn rs (DApp4 mr _rrep rtc _ka  _a kb  b)         rto)
  | ltc /= typeRepTyCon (typeRep @PipeFun) = err $ "INTERNAL: left not a PipeFun"
  | rtc /= typeRepTyCon (typeRep @PipeFun) = err $ "INTERNAL: right not a PipeFun"
  | Nothing <-
    typeRep @Point `eqTypeRep` typeRepKind kpb  = err $ "left a non-traversing Point-wise: " <> showPipe l
  --  | Just _  <-
  --   typeRep @Point `eqTypeRep` typeRepKind kb' = err $ "right a non-traversable Point-wise: " <> showPipe r
  | Nothing <-   b `eqTypeRep`  b' = err $ "ground mismatch: "<>showLRP l r
  --  | Nothing <-  kb`eqTypeRep` kpb = err $ "ground kind mismatch: "<>showLRP l r
  | otherwise
    = doTraverse (Unsafe.unsafeCoerce ml) lpn ls (Unsafe.unsafeCoerce mr) rpn rs
  where
    err x = Left $ "'traverse': "<>x

showLR :: Text -> Text -> Text
showLR l r = "left "<>l<>", right "<>r

showLRP :: Pipe -> Pipe -> Text
showLRP l r = showLR (showPipe l) (showPipe r)

compose :: Pipe -> Pipe -> Either Text Pipe
compose p@PGen{} _
  = Left $ "'compose': left fully saturated: " <> showPipe p
compose
  l@(PLink lpn ls (DApp4 mf _ tc'         kb' b' _kc' _c') lfrom _lto)
  r@(PAny  rpn rs (DApp4 mv _ tc  _ka  _a kb  b)           rto)
  | lfrom /= rto =
    Left $ "'compose': mismatch: "<>showLRP l r
  -- Only doing primitive composition
  | tc == typeRepTyCon (typeRep @PipeFun)
  , tc == tc'
  , Just HRefl <- kb `eqTypeRep` kb'
  , Just HRefl <-  b `eqTypeRep`  b'
  , Just HRefl <- typeRep @K.Type `eqTypeRep` typeRepKind kb
  , Just HRefl <- typeRep @K.Type `eqTypeRep` typeRepKind  b
  = doBind (Unsafe.unsafeCoerce mv) rpn rs (Unsafe.unsafeCoerce mf) lpn ls
  | otherwise
  = Left $ "compose: mismatch: "<>showLRP l r
compose l r
  = Left $ "compose: incompatible: "<>showLRP l r

apply :: Pipe -> [SomeValue] -> Either Text Pipe
apply p@PGen{} _
  =   Left $ "'apply': left fully saturated: " <> showPipe p
apply p []
  =   Right p
apply (PLink rpn rs (DApp4 mf _ _tc' kb' b' _kc' _c') _rfrom _rto)
      (SomeValue (SomeKindValue (v :: Value ka a) :: SomeKindValue a):xs)
  | Just HRefl <- typeRep @ka `eqTypeRep` kb'
  , Just HRefl <- typeRep  @a `eqTypeRep`  b'
  = join $ flip apply xs <$> doApply (Unsafe.unsafeCoerce mf) rpn rs v


-- * Ops: wrapping IR
--

-- | 'doTraverse': approximate 'traverse':
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- ..with the difference that we should handle a FLink on the right as well,
-- ..but not just yet.
doTraverse
  :: forall  ka a kb kpb b kpc c rb
   . ( kpb ~ Point
     , kpc ~ Point
     , rb ~ Repr  kb b)
  => PipeFun     kpb b kpc c -> Name Pipe -> Struct
  -> PipeFun ka a kb b       -> Name Pipe -> Struct
  -> Either Text Pipe
doTraverse
  (FLink   tagTo   (f :: b -> Result c))  (Name ln) (Struct lg)
  (FOutput (tagFrom :: Tag kb b) (t ::      Result rb)) (Name rn) (Struct rg)
  = Right $ Pipe (Name $ "("<>ln<>")-<trav>-("<>rn<>")") sig struct tagTo dyn
  where ty      = proxyType (Proxy @kb) (Proxy @c)
        sig     = Gen ty
        struct  = Struct graph
        graph   = lg `G.overlay` rg -- XXX: structure!
        dyn     = Dynamic (typeRep :: TypeRep (PipeFun Point () kb c)) pipeFun
        tagRes  = tagOtherGround (Proxy @c) tagFrom
        pipeFun = FOutput tagRes (traversePipes tagFrom f t)
                  :: PipeFun Point () kb c
doTraverse l _ _ r _ _
  = Left $ "doBind: PipeFuns, but "<>showLR (pack $ show l) (pack $ show r)

-- | 'doBind': approximate 'bind':
-- (>>=) :: forall a b. m a -> (a -> m b) -> m b infixl 1 Source #
-- ..with the difference that we should handle a FLink on the left as well,
-- ..but not just yet.
doBind
  :: forall  ka a kb b kc c rb rc
   . ( rb ~ Repr kb b
     , rc ~ Repr kc c)
  => PipeFun ka a kb b      -> Name Pipe -> Struct
  -> PipeFun      kb b kc c -> Name Pipe -> Struct
  -> Either Text Pipe
doBind (FOutput _tagFrom (v :: Result rb))  (Name rn) (Struct rg)
       (FLink tagTo (f :: lb -> Result lc)) (Name ln) (Struct lg)
  = Right $ Pipe (Name $ ln<>">>="<>rn) sig struct tagTo dyn
  where ty      = proxyType (Proxy @kc) (Proxy @c)
        sig     = Gen ty
        struct  = Struct graph
        graph   = G.overlay lg rg
        dyn     = Dynamic (typeRep :: TypeRep (PipeFun Point () kc c)) pipeFun
        pipeFun = FOutput tagTo (bindPipes v f) :: PipeFun Point () kc c
doBind l _ _ r _ _
  = Left $ "doBind: PipeFuns, but "<>showLR (pack $ show l) (pack $ show r)

-- | 'doApply': approximate 'apply':
-- ($) :: (a -> b) -> a -> b
doApply
  :: forall  ka a kb b ra rb
   . ( ra ~ Repr ka a
     , rb ~ Repr kb b
     , Ground a)
  => PipeFun     ka a kb b -> Name Pipe -> Struct
  -> Value       ka a
  -> Either Text Pipe
doApply (FLink tagTo (f :: ra -> Result rb)) (Name rn) (Struct rg)
        v
  = Right $ Pipe (Name $ "app-"<>rn) sig struct tagTo dyn
  where ty      = proxyType (Proxy @kb) (Proxy @b)
        sig     = Gen ty
        struct  = Struct graph
        graph   = rg -- XXX ???
        dyn     = Dynamic (typeRep :: TypeRep (PipeFun Point () kb b)) pipeFun
        pipeFun = FOutput tagTo (applyPipe f v) :: PipeFun Point () kb b
doApply v f _ _ = Left $ "doApply: PipeFun+Value, but "<>showLR (pack $ show f) (pack $ show v)


-- * Ops:  actual core
--
-- XXX: ExceptT

-- | 'traversePipes': approximate 'traverse':
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
traversePipes
  :: forall a b klb kra
  . ( Ground a, Ground b
    , klb ~ Point)
  => Tag kra a
  -> (a -> Result (Repr klb b))
  -> Result (Repr kra a)
  -> Result (Repr kra b)
traversePipes kra f t = do
  tv <- t
  case tv :: Either Text (Repr kra a) of
    Left e -> pure $ Left e
    Right (x :: Repr kra a) ->
      case kra of
        TPoint -> f x
        TList  -> sequence <$> traverse f x
        TSet   -> (listSetUnsafe <$>) . sequence <$> traverse f (setToList x)
        TTree  -> pure $ Left "traverse: Tree unsupported"
        TDag   -> pure $ Left "traverse: Dag unsupported"
        TGraph -> pure $ Left "traverse: Graph unsupported"

-- | 'applyPipe': approximate 'apply':
-- ($) :: (a -> b) -> a -> b
applyPipe
  :: forall k a ra rb
  . ( ra ~ Repr k a, Ground a)
  => (ra -> Result rb)
  -> Value k a
  -> Result rb
applyPipe f = \case
  VPoint x -> f x
  VList  x -> f x
  VSet   x -> f x
  VTree  x -> f x
  VDag   x -> f x
  VGraph x -> f x

-- | 'bindPipes': approximate 'bind':
-- (>>=) :: forall a b. m a -> (a -> m b) -> m b infixl 1 Source #
bindPipes
  :: Result a
  -> (a -> Result b)
  -> Result b
bindPipes v f = do
  r <- v
  case r of
    Left e  -> pure $ Left e
    Right x -> f x


-- * Boring
--
instance Show (PipeFun ka a kb b) where
  show FOutput{} = "FOutput Point () "<>show (typeRep @kb)<>" "<>show (typeRep @b)
  show FLink{}   = "FLink "<>show (typeRep @a)<>" "<>show (typeRep @b)
