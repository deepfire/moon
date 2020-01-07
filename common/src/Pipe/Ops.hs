module Pipe.Ops
  ( Ops(..)
  , opsFull
  , opsDesc
  -- * High-level
  , compile
  , resolveNames
  -- * Ops
  , runPipe
  , apply
  , compose
  , traverseP
  -- * Constructors
  , gen, genG, gen'
  , link, linkG, link'
  , emptyDesc
  , emptyPipe
  , someEmptyPipe
  )
where

import qualified Algebra.Graph                    as G
import           Data.Dynamic                       (fromDynamic)
import qualified Data.Kind                        as K
import           Data.Maybe                         (fromJust)
import           Type.Reflection

import Basis
import Pipe.Expr
import Pipe.Types
import Pipe.Zipper
import Type


-- * Operations of pipe gut composition
--
instance PipeOps Dynamic where
  pipeOps = opsFull

instance PipeOps () where
  pipeOps = opsDesc

opsFull :: Ops Dynamic
opsFull = Ops
  { app  = appDyn
  , trav = travDyn
  , comp = compDyn
  }

opsDesc :: Ops ()
opsDesc = Ops
  { app  = const . const . const ()
  , trav = const . const . const . const ()
  , comp = const . const . const . const ()
  }


-- * Compiling pipe expressions
--
compile
  :: forall m e p
  . (Monad m, e ~ Text)
  => Ops p
  -> (QName Pipe -> m (Maybe (SomePipe p)))
  -> Expr (QName Pipe)
  -> m (Either e (SomePipe p))
compile ops lookupPipe expr =
  (sequence <$> resolveNames lookupPipe expr)
    <&>
  (assemble ops <$>)
    <&>
  join . mapLeft failMissing
 where
   failMissing = ("No such pipe: " <>) . showQName

data TCR p
  = Known (SomePipe p)

checkAndInfer
  :: forall m e p
  .  (Monad m, e ~ Text)
  => Expr (Either (QName Pipe) (SomePipe p))
  -> Either e (Expr (Either (SomePipe ()) (SomePipe p)))
checkAndInfer expr = go True $ fromExpr expr
 where
   toSig :: Either (SomePipe ()) (SomePipe p) -> (SomePipe ())
   toSig (Left x)  = x
   toSig (Right p) = somePipeDesc p
   go :: Bool
      -> ZExpr (Either (QName Pipe) (SomePipe p))
      -> Either e (Expr (Either (SomePipe ()) (SomePipe p)))
   go _ (_, PVal x)          = Right (PVal x)
   go known z@(parents, PPipe p) = case p of
     Right x   -> Right . PPipe $ Right x
     Left name -> case parents of
       [] -> Left $ "Lacking context to infer unknown: " <> showQName name
       Right PApp{}:_  -> PPipe . Left <$> inferFnApp name z
       Left  PApp{}:_  -> undefined
       Right PComp{}:_ -> undefined
       Left  PComp{}:_ -> undefined
       _ -> Left $ "Child of an atom: " <> showQName name
   inferFnApp
     :: QName Pipe
     -> ZExpr (Either (QName Pipe) (SomePipe p))
     -> Either e (SomePipe ())
   inferFnApp n = goIFA [] where
     goIFA :: [SomePipe ()]
           -> ZExpr (Either (QName Pipe) (SomePipe p))
           -> Either e (SomePipe ())
     goIFA xs z@(Right (PApp f x):ps, self) = join $
       goIFA <$> ((:)
                  <$> undefined (go False (fromExpr x))
                  <*> pure xs)
             <*> pure (upFromLeft' z)
     goIFA xs _ = undefined xs

resolveNames
  :: forall m e p
  .  (Monad m, e ~ Text)
  => (QName Pipe -> m (Maybe (SomePipe p)))
  -> Expr (QName Pipe)
  -> m (Expr (Either (QName Pipe) (SomePipe p)))
resolveNames lookupPipe = traverse tryLookupPipe
  where
    tryLookupPipe :: QName Pipe -> m (Either (QName Pipe) (SomePipe p))
    tryLookupPipe name = maybeToEither name <$> lookupPipe name

assemble
  :: forall e p. e ~ Text
  => Ops p
  -> Expr (SomePipe p)
  -> Either e (SomePipe p)
assemble Ops{app, comp, trav} = go
  where
    go :: Expr (SomePipe p) -> Either Text (SomePipe p)
    go (PPipe p) = Right p

    go (PApp    PVal{}  _)          = Left $ "Applying a value."
    go (PApp  f           PVal{vX}) = join $ apply app <$> go f <*> pure vX
    -- here we expect traverseP to reduce:
    --   PApp (b -> IO c) (a -> IO b) -> (a -> IO c)
    go (PApp  f@PPipe{} x@PPipe{})  =        traverseP trav          (pP f)    (pP x)
    go (PApp  f@PPipe{} x@PComp{})  = join $ traverseP trav <$> pure (pP f) <*> go x
    go (PApp  f         x)          = join $ traverseP trav <$> go f        <*> go x

    go (PComp   PVal{}  _)          = Left $ "Composing a value on the left."
    go (PComp _           PVal{})   = Left $ "Composing a value on the right."
    go (PComp l@PPipe{} r@PPipe{})  = compose comp (pP l) (pP r)
    go (PComp l         r)          = join $ compose comp <$> (go l) <*> (go r)

    go PVal{} = Left "Processing a value should never happen."


-- * Constructors
--
genG
  :: forall kt tt
  .  (ReifyTag kt, Typeable (Repr kt tt), Typeable kt, Ground tt)
  => Name Pipe
  -> Type kt tt
  -> Result (Repr kt tt)
  -> SomePipe Dynamic
genG n to pf = G $ gen' n to pf

gen
  :: forall kt tt
  .  (ReifyTag kt, Typeable (Repr kt tt), Typeable kt, Typeable tt)
  => Name Pipe
  -> Type kt tt
  -> Result (Repr kt tt)
  -> SomePipe Dynamic
gen n to pf = T $ gen' n to pf

linkG
  :: forall kf tf kt tt
  . ( ReifyTag kf,ReifyTag kt, Typeable (Repr kf tf), Typeable (Repr kt tt)
    , Typeable kf, Typeable tf, Typeable kt
    , Ground tt)
  => Name Pipe
  -> Type kf tf
  -> Type kt tt
  -> (Repr kf tf -> Result (Repr kt tt))
  -> SomePipe Dynamic
linkG n from to pf = G $ link' n from to pf

link
  :: forall kf tf kt tt
  . ( ReifyTag kf, ReifyTag kt, Typeable (Repr kf tf), Typeable (Repr kt tt)
    , Typeable kf, Typeable tf, Typeable kt
    , Typeable tt)
  => Name Pipe
  -> Type kf tf
  -> Type kt tt
  -> (Repr kf tf -> Result (Repr kt tt))
  -> SomePipe Dynamic
link n from to pf = T $ link' n from to pf

gen'
  :: forall kf tf kt tt c
  .  ( kf ~ Point, tf ~ ()
     , ReifyTag kt, Typeable (Repr kt tt), Typeable kt, Typeable tt, Typeable c
     , c tt)
  => Name Pipe
  -> Type kt tt
  -> Result (Repr kt tt)
  -> Pipe c kf tf kt tt Dynamic
gen' name (splitType -> (tagTo, pTo)) mv
  -- TODO: validate types agains the typerep/dynamic
                = Pipe desc dyn
  where ty      = tagType tagTo pTo
        desc    = Desc name sig struct (dynRep dyn) TPoint (Proxy @()) tagTo pTo
        sig     = Gen unitType ty
        struct  = Struct graph
        graph   = G.vertex ty
        dyn     = Dynamic typeRep pipeFun
        pipeFun = IOA' (const mv) :: IOA' c Point () kt tt

link'
  :: forall kf tf kt tt c
  . ( ReifyTag kf, ReifyTag kt, Typeable (Repr kf tf), Typeable (Repr kt tt)
    , Typeable kf, Typeable tf, Typeable kt, Typeable tt, Typeable c
    , c tt)
  => Name Pipe
  -> Type kf tf
  -> Type kt tt
  -> (Repr kf tf -> Result (Repr kt tt))
  -> Pipe c kf tf kt tt Dynamic
link' name (splitType -> (kf, tf)) (splitType -> (kt, tt)) mf
                = Pipe desc dyn
  where desc    = Desc name sig struct (dynRep dyn) kf tf kt tt
        sig     = Link (tagType kf tf) (tagType kt tt)
        struct  = Struct $ G.connect (G.vertex $ sIn sig) (G.vertex $ sOut sig)
        ---------
        dyn     = Dynamic typeRep pipeFun
        pipeFun = IOA' mf :: IOA' c kf tf kt tt

emptyDesc :: Name Pipe -> Desc Ground [] ()
emptyDesc name = Desc
  { pdName   = name
  , pdSig    = Gen unitType unitType
  , pdStruct = Struct G.empty
  , pdRep    = SomeTypeRep (typeRep @(IOA' Ground Point () Point ()))
  , pdArgs   = Nil
  , pdOut    = Proxy @()
  }

emptyPipe :: Name Pipe -> p -> Pipe Ground Point () Point () p
emptyPipe = Pipe . emptyDesc

someEmptyPipe :: Name Pipe -> p -> SomePipe p
someEmptyPipe = G .: emptyPipe


-- * Running
--
runPipe :: SomePipe Dynamic -> Result SomeValue
runPipe (T p) = pure . Left $ "runPipe:  non-Ground pipe: " <> showPipe p
runPipe (G Pipe{pDesc, p}) = runPipe' pDesc p

runPipe'
  :: forall c (as :: [*]) o. PipeConstr c as o
  => Desc c as o
  -> Dynamic
  -> Result SomeValue
runPipe' pd@Desc{pdArgs, pdOut} dyn =
  case fromDynamic dyn :: Maybe (IOA' Ground Point () kb b) of
    Nothing -> pure . Left $ "Incomplete pipe: " <> showDesc pd
    Just (IOA' io) ->
      (SomeValue . SomeKindValue pdToK . mkValue pdTo pdToK <$>) <$> io ()


-- * Basic ops:  unwrapping Dynamics
--
-- | 'traverseP': approximate 'traverse':
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
type family TraverseC (c1 :: * -> Constraint) (c2 :: * -> Constraint) where
  TraverseC Ground _ = Ground
  TraverseC x      _ = x

traverseP
  :: (forall c1 c2 t tt1 kf2 tf2 kt2
      . ( Typeable c1, Typeable c2, c1 (), c1 tt1
        , Typeable t, Typeable tt1, Typeable kf2, Typeable tf2, Typeable kt2
        , ReifyTag kf2, ReifyTag kt2)
      => Desc c1         Point t Point tt1 -> p
      -> Desc c2 kf2 tf2 kt2   t           -> p
      -> p)
  -> SomePipe p -> SomePipe p -> Either Text (SomePipe p)
traverseP pf (G f) (G t) = G <$> traverseP'' pf f t
traverseP pf (G f) (T t) = G <$> traverseP'' pf f t
traverseP pf (T f) (T t) = T <$> traverseP'' pf f t
traverseP pf (T f) (G t) = T <$> traverseP'' pf f t

traverseP''
  :: forall c1 c2 kf1 tf1 kt1 tt1 kf2 tf2 kt2 tt2 p
  .  ( Typeable c1, Typeable c2, c1 (), c1 tt1
     , Typeable kf1, Typeable tf1, Typeable kt1, Typeable tt1
     , Typeable kf2, Typeable tf2, Typeable kt2, Typeable tt2
     , ReifyTag kf2, ReifyTag kt2)
  => (forall c1 c2 t tt1 kf2 tf2 kt2
      . ( Typeable c1, Typeable c2, c1 (), c1 tt1
        , Typeable t, Typeable tt1, Typeable kf2, Typeable tf2, Typeable kt2
        , ReifyTag kf2, ReifyTag kt2)
      => Desc c1         Point t Point tt1 -> p
      -> Desc c2 kf2 tf2 kt2   t           -> p
      -> p)
  -> Pipe c1         kf1 tf1 kt1 tt1 p
  -> Pipe c2 kf2 tf2 kt2 tt2         p
  -> Either Text (Pipe (TraverseC c1 c2) kf2 tf2 kt2 tt1 p)
traverseP'' pf
  | Just HRefl <- typeRep @Point `eqTypeRep` typeRep @kf1
  , Just HRefl <- typeRep @Point `eqTypeRep` typeRep @kt1
  , Just HRefl <- typeRep @tf1   `eqTypeRep` typeRep @tt2
  = traverseP' pf
  | otherwise
  = \l r -> Left $ ("'traverseP' failed on: l="<>pack (show l)<>" r="<>pack (show r))

traverseP'
  :: forall c1 c2 t tt1 kf2 tf2 kt2 p
  .  ( Typeable c1, Typeable c2, c1 (), c1 tt1
     , Typeable kt2, Typeable tt1, Typeable kf2, Typeable tf2, Typeable t
     , ReifyTag kf2, ReifyTag kt2)
  => (Desc c1 Point t Point tt1 -> p -> Desc c2 kf2 tf2 kt2 t -> p -> p)
  -> Pipe c1         Point t Point tt1 p
  -> Pipe c2 kf2 tf2 kt2   t p
  -> Either Text (Pipe (TraverseC c1 c2) kf2 tf2 kt2 tt1 p)
-- traverseP' _ p@PGen{} _
--   =   Left $ "'traverse': left fully saturated: " <> showPipe p
traverseP' pf
  l@(P _ _ (App4 ltc   kpb b' _ _) _ _ _)
  r@(P  _ _ (App4 rtc _ _ _ b)        _ _)
  | ltc /= typeRepTyCon (typeRep @IOA') = err $ "INTERNAL: left not a PipeFun"
  | rtc /= typeRepTyCon (typeRep @IOA') = err $ "INTERNAL: right not a PipeFun"
  | Nothing <-
    typeRep @Point `eqTypeRep` typeRepKind kpb  = err $ "left a non-traversing Point-wise: " <> showPipe l
  --  | Just _  <-
  --   typeRep @Point `eqTypeRep` typeRepKind kb' = err $ "right a non-traversable Point-wise: " <> showPipe r
  | Nothing <-   b `eqTypeRep`  b' = err $ "traverse: ground mismatch: f="<>pack (show l)<>" v="<>pack (show r)
  --  | Nothing <-  kb`eqTypeRep` kpb = err $ "ground kind mismatch: "<>showLRP l r
  | otherwise
    = doTraverse pf l r
  where
    err x = Left $ "'traverse': "<>x

showLR :: Text -> Text -> Text
showLR l r = "left "<>l<>", right "<>r

showLRP :: Pipe c1 kf1 tf1 kt1 tt1 Dynamic -> Pipe c2 kf2 tf2 kt2 tt2 Dynamic -> Text
showLRP l r = showLR (showPipe l) (showPipe r)

type family BindGround (c1 :: * -> Constraint) (c2 :: * -> Constraint) where
  BindGround x _ = x


-- | 'compose': approximate '(.)':
-- (.) :: (b -> c) -> (a -> b) -> a -> c
compose
  :: (forall c1 c2   k t kt1 tt1
             kf2 tf2
      . ( Typeable c1, Typeable c2
        , Typeable kf2, Typeable tf2, Typeable k, Typeable t, Typeable kt1, Typeable tt1
        , ReifyTag k, ReifyTag kt1
        , c1 (), c1 tt1)
      => Desc c2 kf2 tf2 k t -> p
      -> Desc c1         k t kt1 tt1 -> p
      -> p)
  -> SomePipe p
  -> SomePipe p
  -> Either Text (SomePipe p)
  -- ..and here we pay the price of delaying the equality proofs..
compose pf (T l) (G r) = T <$> compose'' pf l r
compose pf (G l) (G r) = G <$> compose'' pf l r
compose pf (T l) (T r) = T <$> compose'' pf l r
compose pf (G l) (T r) = G <$> compose'' pf l r

compose''
  :: forall c1 c2   kf1 tf1 kt1 tt1
            kf2 tf2 kt2 tt2
            p
  . ( Typeable c1, Typeable c2
    , Typeable tf1, Typeable kf1, Typeable kt1, Typeable tt1, Typeable kf2, Typeable tf2, Typeable kt2, Typeable tt2
    , ReifyTag kt1, ReifyTag kt2
    , c1 (), c1 tt1
    )
  => (forall c1 c2   k t kt1 tt1
             kf2 tf2
      . ( Typeable c1, Typeable c2
        , Typeable kf2, Typeable tf2, Typeable k, Typeable t, Typeable kt1, Typeable tt1
        , ReifyTag k, ReifyTag kt1
        , c1 (), c1 tt1)
      => Desc c2 kf2 tf2 k t -> p
      -> Desc c1         k t kt1 tt1 -> p -> p)
  -> Pipe c1         kf1 tf1 kt1 tt1 p
  -> Pipe c2 kf2 tf2 kt2 tt2 p
  -> Either Text (Pipe (BindGround c1 c2) kf2 tf2 kt1 tt1 p)
compose'' pf
  | Just HRefl <- typeRep @kt2 `eqTypeRep` typeRep @kf1
  , Just HRefl <- typeRep @tt2 `eqTypeRep` typeRep @tf1
  = compose' pf
  | otherwise
  = \l r -> Left $ ("'compose' failed on: l="<>pack (show l)<>" r="<>pack (show r))

-- | 'compose': approximate '(.)'
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- ..and convert to
-- (=<<) :: (a -> m b) -> m a -> m b
compose'
  :: forall c1 c2   kf1 tf1 kt1 tt1
            kf2 tf2 kt2 tt2
            resC p
  . ( resC ~ (BindGround c1 c2)
    , Typeable c1, Typeable c2
    , Typeable tf1, Typeable tt1, Typeable kt1, Typeable kf1, Typeable kf2, Typeable tf2
    , ReifyTag kf1, ReifyTag kt1
    , resC (), resC tt1
    , kt2 ~ kf1, tt2 ~ tf1)
  => (Desc c2 kf2 tf2 kt2 tt2 -> p -> Desc c1 kf1 tf1 kt1 tt1 -> p -> p)
  -> Pipe c1 kf1 tf1 kt1 tt1 p
  -> Pipe c2 kf2 tf2 kt2 tt2 p
  -> Either Text (Pipe (BindGround c1 c2) kf2 tf2 kt1 tt1 p)
-- compose' _ p@P{} _
--   = Left $ "'compose': left fully saturated: " <> showPipe p
compose' pf
  f@(P _ _ (App4 tc'     _ _ _ _) _ lfrom _)
  v@(P _ _ (App4 tc _ _ kb b)         _ rto)
  | lfrom /= rto =
    Left $ "'compose': mismatch: f="<>pack (show f)<>" v="<>pack (show v)
  -- Only doing primitive composition
  | tc == typeRepTyCon (typeRep @IOA')
  , tc == tc'
  , Just HRefl <- typeRep @K.Type `eqTypeRep` typeRepKind kb
  , Just HRefl <- typeRep @K.Type `eqTypeRep` typeRepKind  b
  = doBind pf f v
  | otherwise
  = Left $ "compose: mismatch: f="<>pack (show f)<>" v="<>pack (show v)
compose' _ f v
  = Left $ "compose: incompatible: f="<>pack (show f)<>" v="<>pack (show v)


-- * Ops: wrapping IR
--

-- | 'doTraverse': approximate 'traverse':
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- ..with the difference that we should handle a FLink on the right as well,
-- ..but not just yet.
doTraverse
  :: forall c1 c2 resC ka a ra kb b rb c p
   . ( resC ~ TraverseC c1 c2
     , resC (), Typeable resC
     , Typeable c1, Typeable c2, Typeable ka, Typeable a, Typeable b, Typeable c
     -- , ka ~ Point, a ~ ()
     , ra ~ Repr  ka a
     , rb ~ Repr  kb b
     )
  => (Desc c1 Point b Point c -> p -> Desc c2 ka a kb b -> p -> p)
  -> Pipe c1      Point b Point c p
  -> Pipe c2 ka a kb    b         p
  -> Either Text (Pipe (TraverseC c1 c2) ka a kb c p)
doTraverse pf
  (Pipe df@(Desc (Name ln) _ (Struct lg) _  _ _  _ c) f)
  (Pipe dt@(Desc (Name rn) _ (Struct rg) _ ka a kb _) t)
  = Right $ Pipe desc (pf df f dt t)
  where desc    = Desc name sig struct (SomeTypeRep rep) ka a kb c
        name    = Name $ "("<>ln<>")-<trav>-("<>rn<>")"
        sig     = Link (tagType ka a) (tagType kb c) -- XXX: Link
        struct  = Struct (lg `G.overlay` rg) -- XXX: structure!
        rep     = typeRep :: TypeRep (IOA' (TraverseC c1 c2) ka a kb c)
-- XXX: where is the best place for this check now?
-- doTraverse l _ _ r _ _
--   = Left $ "doBind: PipeFuns, but "<>showLR (pack $ show l) (pack $ show r)

-- | 'doBind': approximate 'bind':
-- (=<<) :: (a -> m b) -> m a -> m b
-- ..with the difference that we should handle a FLink on the left as well,
-- ..but not just yet.
doBind
  :: forall c1 c2 resC ka a ra kb b rb kc c rc p
   . ( resC ~ (BindGround c1 c2)
     , resC (), Typeable resC
     , Typeable ka, Typeable a, Typeable b, Typeable c, Typeable c2
     , ra ~ Repr ka a
     , rb ~ Repr kb b
     , rc ~ Repr kc c)
  => (Desc c2 ka a kb b -> p -> Desc c1 kb b kc c -> p -> p)
  -> Pipe c1      kb b kc c p
  -> Pipe c2 ka a kb b      p
  -> Either Text (Pipe resC ka a kc c p)
doBind pf
  (Pipe df@(Desc (Name ln) _ (Struct lg) _ _  _ kc c) f)
  (Pipe dv@(Desc (Name rn) _ (Struct rg) _ ka a  _ _) v)
  = Right $ Pipe desc (pf dv v df f)
  where desc    = Desc name sig struct (SomeTypeRep rep) ka a kc c
        name    = Name $ ln<>">>="<>rn
        sig     = Gen unitType (tagType kc c)
        struct  = Struct $ G.overlay lg rg
        rep     = typeRep :: TypeRep (IOA' resC ka a kc c)
-- XXX: where is the best place for this check now?
-- doBind l _ _ r _ _
--   = Left $ "doBind: PipeFuns, but "<>showLR (pack $ show l) (pack $ show r)


-- * Ops:  actual core
--
-- XXX: ExceptT

compDyn
  :: forall c1 c2 resC ka a kb b kc c
   . ( Typeable c1, Typeable c2
     , Typeable a, Typeable ka, Typeable kb, Typeable b, Typeable kc, Typeable c
     , ReifyTag kb, ReifyTag kc
     , resC ~ (BindGround c1 c2)
     , resC c)
  => Desc c2 ka a kb b      -> Dynamic
  -> Desc c1      kb b kc c -> Dynamic
  -> Dynamic
compDyn _descv v _descf f = Dynamic typeRep pipeFun -- XXX: re-creation of typerep
  where
    IOA' f'  = fromJust $ fromDynamic f :: IOA' c1 kb b kc c
    IOA' v'  = fromJust $ fromDynamic v :: IOA' c2 ka a kb b
    pipeFun = IOA' (bindPipes v' f') :: IOA' resC ka a kc c

travDyn
  :: forall c1 c2 resC ka a kb b c
   . ( resC ~ TraverseC c1 c2
     , resC (), Typeable resC, resC c
     , Typeable c1, Typeable c2
     , Typeable a, Typeable ka, Typeable kb, Typeable b, Typeable c
     , ReifyTag ka, ReifyTag kb)
  => Desc c1      Point b Point c -> Dynamic
  -> Desc c2 ka a kb    b         -> Dynamic
  -> Dynamic
travDyn df f dt t = Dynamic typeRep pipeFun -- XXX: re-creation of typerep
  where
    IOA' f'  = fromJust $ fromDynamic f :: IOA' c1      Point b Point c
    IOA' t'  = fromJust $ fromDynamic t :: IOA' c2 ka a kb    b
    pipeFun = IOA' ioa :: IOA' resC ka a kb c
    ioa     = traversePipes (pdFromK dt) (pdFrom dt) (pdToK dt) (pdTo dt) (pdTo df) f' t'

-- | 'bindPipes': approximate 'bind':
-- (>>=) :: forall a b. m a -> (a -> m b) -> m b
bindPipes
  :: (a -> Result b)
  -> (b -> Result c)
  -> (a -> Result c)
bindPipes v f = \ra -> do
  r <- v ra
  case r of
    Left e  -> pure $ Left e
    Right x -> f x

-- | 'traversePipes': approximate 'traverse':
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
traversePipes
  :: forall ka a kb b c
  . ( Typeable a, Typeable b, Typeable c
    )
  => Tag ka -> Proxy a -> Tag kb -> Proxy b -> Proxy c
  -> (b -> Result c)
  -> (Repr ka a -> Result (Repr kb b))
  -> Repr ka a -> Result (Repr kb c)
traversePipes _ _ kb _ _ f t = \ra -> do
  tv <- t ra
  case tv :: Either Text (Repr kb b) of
    Left e -> pure $ Left e
    Right (x :: Repr kb b) ->
      case kb of
        TPoint -> f x
        TList  -> sequence <$> traverse f x
        TSet   -> pure $ Left "traverse: Set unsupported"
        TTree  -> pure $ Left "traverse: Tree unsupported"
        TDag   -> pure $ Left "traverse: Dag unsupported"
        TGraph -> pure $ Left "traverse: Graph unsupported"

{-------------------------------------------------------------------------------
  Boring.
-------------------------------------------------------------------------------}
instance Show (IOA' c ka a kb b) where
  show IOA'{} =
    "Fun "<>show (typeRep @ka)<>":"<>show (typeRep @a)<>
    " -> "<>show (typeRep @kb)<>":"<>show (typeRep @b)
