module Pipe.Ops
  (
  -- * High-level
    compile
  , resolveNames
  -- * Ops
  , runPipe
  , apply
  , compose
  -- * Constructors
  , gen, genG, gen'
  , link, linkG, link'
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
  :: (Monad m, e ~ Text)
  => (QName SomePipe -> m (Either Text SomePipe))
  -> Expr (QName SomePipe)
  -> m (Either e SomePipe)
compile lookupPipe nameTree =
  join <$> ((assemble <$>) <$> resolveNames lookupPipe nameTree)

resolveNames
  :: (Monad m, e ~ Text)
  => (QName SomePipe -> m (Either Text SomePipe))
  -> Expr (QName SomePipe)
  -> m (Either e (Expr SomePipe))
resolveNames lookupPipe = (sequence <$>) . traverse lookupPipe


-- * The actual type, private.
--
data PipeFun c (ka :: Con) a (kb :: Con) b where
  FOutput
    :: forall c kb b
    . (Typeable kb, Typeable b, c b)
    => Tag kb -> Proxy b
    ->               Result (Repr kb b)  -> PipeFun c 'Point () kb b
  FLink
    :: forall c ka a kb b
    . (Typeable ka, Typeable a, Typeable kb, Typeable b, c b)
    => Tag kb -> Proxy b
    -> (Repr ka a -> Result (Repr kb b)) -> PipeFun c ka a kb b

pattern App4 :: TyCon -> TypeRep a -> TypeRep ka -> TypeRep b -> TypeRep kb -> TypeRep c
pattern App4 con ka a kb b <- App (App (App (App (App (Con con) _) ka) a) kb) b

pattern DApp4 :: c -> TypeRep c -> TyCon -> TypeRep a -> TypeRep ka -> TypeRep b -> TypeRep kb -> Dynamic
pattern DApp4 v tr con ka a kb b <- Dynamic tr@(App4 con ka a kb b) v

assemble
  :: e ~ Text
  => Expr SomePipe
  -> Either e SomePipe
assemble = go
  where
    go :: Expr SomePipe -> Either Text SomePipe
    go (PPipe p) = Right p

    go (PApp    PVal{}  _)          = Left $ "Applying a value."
    go (PApp  f           PVal{vX}) = join $ apply <$> go f <*> pure vX
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
genG
  :: forall kt tt
  .  (ReifyTag kt, Typeable (Repr kt tt), Typeable kt, Ground tt)
  => Name Pipe
  -> Tag2 kt tt
  -> Result (Repr kt tt)
  -> SomePipe
genG n to pf = G $ gen' n to pf

gen
  :: forall kt tt
  .  (ReifyTag kt, Typeable (Repr kt tt), Typeable kt, Typeable tt)
  => Name Pipe
  -> Tag2 kt tt
  -> Result (Repr kt tt)
  -> SomePipe
gen n to pf = T $ gen' n to pf

linkG
  :: forall kf tf kt tt
  . ( ReifyTag kf,ReifyTag kt, Typeable (Repr kf tf), Typeable (Repr kt tt)
    , Typeable kf, Typeable tf, Typeable kt
    , Ground tt)
  => Name Pipe
  -> Tag2 kf tf
  -> Tag2 kt tt
  -> (Repr kf tf -> Result (Repr kt tt))
  -> SomePipe
linkG n from to pf = G $ link' n from to pf

link
  :: forall kf tf kt tt
  . ( ReifyTag kf, ReifyTag kt, Typeable (Repr kf tf), Typeable (Repr kt tt)
    , Typeable kf, Typeable tf, Typeable kt
    , Typeable tt)
  => Name Pipe
  -> Tag2 kf tf
  -> Tag2 kt tt
  -> (Repr kf tf -> Result (Repr kt tt))
  -> SomePipe
link n from to pf = T $ link' n from to pf

gen'
  :: forall kt tt c
  .  ( ReifyTag kt, Typeable (Repr kt tt), Typeable kt, Typeable tt, Typeable c
     , c tt)
  => Name Pipe
  -> Tag2 kt tt
  -> Result (Repr kt tt)
  -> Pipe c
gen' name (splitTag2 -> (tagTo, pTo)) mv
                = Pipe name sig struct tagTo pTo dyn
  where ty      = tagType tagTo pTo
        sig     = Gen unitType ty
        struct  = Struct graph
        graph   = G.vertex ty
        dyn     = Dynamic typeRep pipeFun
        pipeFun = FOutput tagTo pTo mv :: PipeFun c Point () kt tt

link'
  :: forall kf tf kt tt c
  . ( ReifyTag kf, ReifyTag kt, Typeable (Repr kf tf), Typeable (Repr kt tt)
    , Typeable kf, Typeable tf, Typeable kt, Typeable tt, Typeable c
    , c tt)
  => Name Pipe
  -> Tag2 kf tf
  -> Tag2 kt tt
  -> (Repr kf tf -> Result (Repr kt tt))
  -> Pipe c
link' name (splitTag2 -> (tagFrom, pFrom)) (splitTag2 -> (tagTo, pTo)) mf
                = Pipe name sig struct tagTo pTo dyn
  where tyFrom  = tagType tagFrom pFrom
        tyTo    = tagType tagTo pTo
        sig     = Link tyFrom tyTo
        struct  = Struct graph
        graph   = G.connect (G.vertex tyFrom) (G.vertex tyTo)
        dyn     = Dynamic typeRep pipeFun
        pipeFun = FLink tagTo pTo mf :: PipeFun c kf tf kt tt


-- * Running
--
runPipe :: SomePipe -> Result SomeValue
runPipe (T p) = pure . Left $ "runPipe:  non-Ground pipe: " <> showPipe p
runPipe (G Pipe{pTag, pTo, pDyn}) = runPipe' pTag pTo pDyn

runPipe' :: forall (k :: Con) (a :: *)
         . ( Typeable k, Ground a)
         => Tag k -> Proxy a
         -> Dynamic
         -> Result SomeValue
runPipe' _tagTo _pTo dyn =
  case fromDynamic dyn :: Maybe (PipeFun Ground Point () k a) of
    Nothing -> pure . Left $ "Incomplete pipe: " <> pack ""
    Just (FOutput tagTo pTo pipeFun) -> do
      (SomeValue . SomeKindValue . mkValue pTo tagTo <$>) <$> pipeFun


-- * Basic ops:  unwrapping Dynamics
--
-- | 'traverseP': approximate 'traverse':
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
type family TraverseC (c1 :: * -> Constraint) (c2 :: * -> Constraint) where
  TraverseC Ground _ = Ground
  TraverseC x      _ = x

traverseP :: SomePipe -> SomePipe -> Either Text SomePipe
traverseP (G f) (G t) = G <$> traverseP' f t
traverseP (G f) (T t) = G <$> traverseP' f t
traverseP (T f) (T t) = T <$> traverseP' f t
traverseP (T f) (G t) = T <$> traverseP' f t

traverseP'
  :: Typeable c1
  => Pipe c1 -> Pipe c2 -> Either Text (Pipe (TraverseC c1 c2))
traverseP' p@PGen{} _
  =   Left $ "'traverse': left fully saturated: " <> showPipe p
traverseP'
  l@(PLink lpn ls (DApp4 ml _lrep ltc          kpb b' _kc _c) _lfrom _lto)
  r@(PAny  rpn rs (DApp4 mr _rrep rtc _ka  _a _kb  b)         _rto)
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

showLRP :: Pipe c1 -> Pipe c2 -> Text
showLRP l r = showLR (showPipe l) (showPipe r)

type family BindGround (c1 :: * -> Constraint) (c2 :: * -> Constraint) where
  BindGround x _ = x

-- | 'compose': approximate '(.)':
-- (.) :: (b -> c) -> (a -> b) -> a -> c
compose :: SomePipe -> SomePipe -> Either Text SomePipe
compose (T l) (G r) = T <$> compose' l r
compose (G l) (G r) = G <$> compose' l r
compose (T l) (T r) = T <$> compose' l r
compose (G l) (T r) = G <$> compose' l r

-- | 'compose': approximate '(.)'
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- ..and convert to
-- (=<<) :: (a -> m b) -> m a -> m b
compose'
  :: Typeable c1
  => Pipe c1 -> Pipe c2 -> Either Text (Pipe (BindGround c1 c2))
compose' p@PGen{} _
  = Left $ "'compose': left fully saturated: " <> showPipe p
compose'
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
  = doBind (Unsafe.unsafeCoerce mf) lpn ls (Unsafe.unsafeCoerce mv) rpn rs
  | otherwise
  = Left $ "compose: mismatch: "<>showLRP l r
compose' l r
  = Left $ "compose: incompatible: "<>showLRP l r

apply :: SomePipe -> SomeValue -> Either Text SomePipe
apply (G p) x = G <$> apply' p x
apply (T p) x = T <$> apply' p x

apply'
  :: Typeable c
  => Pipe c -> SomeValue -> Either Text (Pipe c)
apply' p@PGen{} _
  =   Left $ "'apply': left fully saturated: " <> showPipe p
apply' (PLink rpn rs (DApp4 mf _ _tc' kb' b' _kc' _c') _rfrom _rto)
      (SomeValue (SomeKindValue (v :: Value ka a) :: SomeKindValue a))
  | Nothing <- typeRep @ka `eqTypeRep` kb'
  = Left $ "Apply: Con mismatch: "   <> show2 "ka" (typeRep @ka) "kb'" kb'

  | Nothing <- typeRep  @a `eqTypeRep`  b'
  = Left $ "Apply: Value mismatch: " <> show2  "a" (typeRep  @a)  "b'"  b'

  | Just HRefl <- typeRep @ka `eqTypeRep` kb'
  , Just HRefl <- typeRep  @a `eqTypeRep`  b'
  = doApply (Unsafe.unsafeCoerce mf) rpn rs v
  | otherwise
  = Left "Fall through."
  where
    show2 :: Text -> TypeRep l -> Text -> TypeRep r -> Text
    show2 ln l rn r = ln<>"="<>pack (show l)<>", "<>rn<>"="<>pack (show r)

apply' (PLink _ _ (Dynamic tr _) _ _) _ =
  Left $ "Typerep structure mismatch: " <> pack (show tr)


-- * Ops: wrapping IR
--

-- | 'doTraverse': approximate 'traverse':
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- ..with the difference that we should handle a FLink on the right as well,
-- ..but not just yet.
doTraverse
  :: forall c1 c2 ka a kb kpb b kpc c rb
   . ( kpb ~ Point
     , kpc ~ Point
     , rb ~ Repr  kb b
     , Typeable c1
     )
  => PipeFun c1     kpb b kpc c -> Name Pipe -> Struct
  -> PipeFun c2 ka a kb b       -> Name Pipe -> Struct
  -> Either Text (Pipe (TraverseC c1 c2))
doTraverse
  (FLink   cT c (f :: b -> Result c))  (Name ln) (Struct lg)
  (FOutput bT b (t ::      Result rb)) (Name rn) (Struct rg)
  = Right $ Pipe (Name $ "("<>ln<>")-<trav>-("<>rn<>")") sig struct cT c dyn
  where ty      = proxyType (Proxy @kb) (Proxy @c)
        sig     = Gen unitType ty
        struct  = Struct graph
        graph   = lg `G.overlay` rg -- XXX: structure!
        dyn     = Dynamic rep pipeFun
        rep     = typeRep :: TypeRep (PipeFun (TraverseC c1 c2) Point () kb c)
        pipeFun = FOutput bT c (traversePipes bT b f t)
                  :: PipeFun (TraverseC c1 c2) Point () kb c
doTraverse l _ _ r _ _
  = Left $ "doBind: PipeFuns, but "<>showLR (pack $ show l) (pack $ show r)

-- | 'doBind': approximate 'bind':
-- (=<<) :: (a -> m b) -> m a -> m b
-- ..with the difference that we should handle a FLink on the left as well,
-- ..but not just yet.
doBind
  :: forall c1 c2 resC ka a kb b kc c rb rc
   . ( rb ~ Repr kb b
     , rc ~ Repr kc c
     , resC ~ (BindGround c1 c2)
     , Typeable resC)
  => PipeFun c1      kb b kc c -> Name Pipe -> Struct
  -> PipeFun c2 ka a kb b      -> Name Pipe -> Struct
  -> Either Text (Pipe resC)
doBind (FLink cT c (f :: lb -> Result lc)) (Name ln) (Struct lg)
       (FOutput _bT _b (v :: Result rb))  (Name rn) (Struct rg)
  = Right $ Pipe (Name $ ln<>">>="<>rn) sig struct cT c dyn
  where ty      = proxyType (Proxy @kc) c
        sig     = Gen unitType ty
        struct  = Struct graph
        graph   = G.overlay lg rg
        dyn     = Dynamic (typeRep :: TypeRep (PipeFun resC Point () kc c)) pipeFun
        pipeFun = FOutput cT c (bindPipes v f) :: PipeFun resC Point () kc c
doBind l _ _ r _ _
  = Left $ "doBind: PipeFuns, but "<>showLR (pack $ show l) (pack $ show r)

-- | 'doApply': approximate 'apply':
-- ($) :: (a -> b) -> a -> b
doApply
  :: forall c ka a kb b ra rb
   . ( ra ~ Repr ka a
     , rb ~ Repr kb b
     , Typeable c)
  => PipeFun   c ka a kb b -> Name Pipe -> Struct
  -> Value       ka a
  -> Either Text (Pipe c)
doApply (FLink bT b (f :: ra -> Result rb)) (Name rn) (Struct rg)
        v
  = Right $ Pipe (Name $ "app-"<>rn) sig struct bT b dyn
  where ty      = proxyType (Proxy @kb) (Proxy @b)
        sig     = Gen unitType ty
        struct  = Struct graph
        graph   = rg -- XXX ???
        dyn     = Dynamic (typeRep :: TypeRep (PipeFun c Point () kb b)) pipeFun
        pipeFun = FOutput bT b (applyPipe f v) :: PipeFun c Point () kb b
doApply v f _ _ = Left $ "doApply: PipeFun+Value, but "<>showLR (pack $ show f) (pack $ show v)


-- * Ops:  actual core
--
-- XXX: ExceptT

-- | 'traversePipes': approximate 'traverse':
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
traversePipes
  :: forall a b klb kra
  . ( Typeable a, Typeable b
    , klb ~ Point)
  => Tag kra -> Proxy a
  -> (a -> Result (Repr klb b))
  -> Result (Repr kra a)
  -> Result (Repr kra b)
traversePipes kra _a f t = do
  tv <- t
  case tv :: Either Text (Repr kra a) of
    Left e -> pure $ Left e
    Right (x :: Repr kra a) ->
      case kra of
        TPoint -> f x
        TList  -> sequence <$> traverse f x
        TSet   -> pure $ Left "traverse: Set unsupported"
        TTree  -> pure $ Left "traverse: Tree unsupported"
        TDag   -> pure $ Left "traverse: Dag unsupported"
        TGraph -> pure $ Left "traverse: Graph unsupported"

-- | 'applyPipe': approximate 'apply':
-- ($) :: (a -> b) -> a -> b
applyPipe
  :: forall k a ra rb
  . ( ra ~ Repr k a)
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

{-------------------------------------------------------------------------------
  Boring.
-------------------------------------------------------------------------------}
instance Show (PipeFun c ka a kb b) where
  show FOutput{} = "FOutput Point () "<>show (typeRep @kb)<>" "<>show (typeRep @b)
  show FLink{}   = "FLink "<>show (typeRep @a)<>" "<>show (typeRep @b)
