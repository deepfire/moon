module Pipe.Ops.Apply
  (apply
  )
where

import           Data.Dynamic                       (fromDynamic)
import qualified Data.Kind                        as K
import           Data.Maybe                         (fromJust)
import qualified Data.SOP                         as SOP
import qualified Data.SOP.Constraint              as SOP
import qualified Generics.SOP                     as SOP
import qualified Generics.SOP.NP                  as SOP
import qualified Generics.SOP.NS                  as SOP
import           Type.Reflection

import Basis
import Pipe.Expr
import Pipe.Types
import Pipe.Zipper
import Pipe.Ops.Internal
import Type

apply
  :: (forall c kas kas' o k a
      . ( PipeConstr c kas o
        , kas ~ (Type k a : kas')
        )
       -- how do we prove the tail is typeable?
       -- why do we need the tail to be typeable?
       -- ISTR there was something unavoidable, buy maybe
       -- some discovery is in order once more?
      => Desc c kas o -> Value k a -> p -> p)
  -> SomePipe p
  -> SomeValue
  -> Either Text (SomePipe p)
apply pf (G p) x = apply' pf p x <&> mkSomeGroundPipe
apply pf (T p) x = apply' pf p x <&> mkSomeTopPipe

mkSomeGroundPipe
  :: forall kas o p. (PipeConstr Ground kas o)
  => Pipe Ground kas o p -> SomePipe p
mkSomeGroundPipe p@(pdArgs . pDesc -> SOP.Nil) = GS p
-- mkSomeGroundPipe out args@(_ SOP.:* _) name sig struct rep =
--   G  $ Pipe (Desc name sig struct rep args    out :: Desc Ground args out) ()

mkSomeTopPipe
  :: forall kas o p. (PipeConstr Top kas o)
  => Pipe Top kas o p -> SomePipe p
mkSomeTopPipe p@(pdArgs . pDesc -> SOP.Nil) = TS p
--   TS $ Pipe (Desc name sig struct rep SOP.Nil out :: Desc Top    args out) ()
-- mkSomeTopPipe out args@(_ SOP.:* _) name sig struct rep =
--   T  $ Pipe (Desc name sig struct rep args    out :: Desc Top    args out) ()

apply'
  :: forall c (a1k :: Con) (a1 :: *) (kas :: [*]) (kas' :: [*]) o p
  . ( PipeConstr c kas o
    , kas ~ (Type a1k a1:kas')
    -- , Typeable kas'
    -- , Typeable a1k, Typeable a1
    )
  => (Desc c kas o -> Value a1k a1 -> p -> p)
  -> Pipe c kas o p
  -> SomeValue
  -> Either Text (Pipe c kas' o p)
apply' pf
  f@(P _ _ (App4 _ kb' b' _ _) _ _ _)
    (SomeValue (SomeKindValue _ (v :: Value k a) :: SomeKindValue a))
  | Nothing <- typeRep @k `eqTypeRep` kb'
  = Left $ "Apply: Con mismatch: "   <> show2 "ka" (typeRep @k) "kb'" kb'

  | Nothing <- typeRep  @a `eqTypeRep`  b'
  = Left $ "Apply: Value mismatch: " <> show2  "a" (typeRep  @a)  "b'"  b'

  | Just HRefl <- typeRep @k `eqTypeRep` kb'
  , Just HRefl <- typeRep @k `eqTypeRep` typeRep @a1k
  , Just HRefl <- typeRep @a `eqTypeRep`  b'
  , Just HRefl <- typeRep @a `eqTypeRep` typeRep @a1
  = doApply pf f v
  | otherwise
  = Left "Apply: fall through."
  where
    show2 :: Text -> TypeRep l -> Text -> TypeRep r -> Text
    show2 ln l rn r = ln<>"="<>pack (show l)<>", "<>rn<>"="<>pack (show r)

apply' _ (P _ _ tr _ _ _) _ =
  Left $ "Typerep structure mismatch: " <> pack (show tr)

-- | 'doApply': approximate 'apply':
-- ($) :: (a -> b) -> a -> b
doApply
  :: forall c as o ka a ra rb p
   . ( PipeConstr c as o
     , Typeable (Tail as))
  => (Desc c as o -> Value ka a -> p -> p)
  -> Pipe  c as o p
  -> Value   ka a
  -> Either Text (Pipe c (Tail as) o p)
doApply pf
        (Pipe desc@(Desc (Name rn) (Sig ras ro) (Struct rg) _ (a SOP.:* ass) o) f)
        v
  = Right $ Pipe desc' (pf desc v f)
  where desc'   = Desc name sig struct (SomeTypeRep rep) ass o
        name    = Name $ "app-"<>rn
        sig     = Sig (tail ras) ro
        struct  = Struct rg -- XXX ???
        rep     = typeRep :: TypeRep (IOA c (Tail as) o)

appDyn
  :: forall c as ass (o :: *) f k a f'
   . ( PipeConstr c as o
     , as ~ (TypePair (Type k a):ass)
     , Typeable ass
     )
  => Proxy o -> Desc c as o -> Value k a -> Dynamic
  -> Dynamic
appDyn _
  Desc {pdArgs = (TypePair _ _-- (t :: Tag k) (a :: Proxy a)
                 ) SOP.:* _}
  v ioaDyn = Dynamic typeRep pipeFun
 where
   -- We've already checked the dynamic:
   pipeFun = case fromDynamic ioaDyn of
     Just (ioa :: IOA c as o) -> applyIOA ioa v

applyIOA
  :: forall c as ass o k a
  .  ( PipeConstr c as o
     , as ~ (TypePair (Type k a) : ass)
     , Typeable ass -- IOA's PipeConstr wants it
     )
  => IOA c as  o
  -> Value k a
  -> IOA c ass o
applyIOA
  (IOA (f :: PipeFunTy (TypePair (Type k a):ass) o)
    c as o
  ) v =
  (IOA (applyPipeFun' f (Proxy @ass) o v :: PipeFunTy ass o)
    c (Proxy @ass) (Proxy @o))

-- | 'applyPipeFun': approximate 'apply':
-- ($) :: (a -> b) -> a -> b
applyPipeFun'
  :: forall (as :: [*]) (o :: *) (k :: Con) (a :: *)
  .  PipeFunTy (TypePair (Type k a):as) o
  -> Proxy as
  -> Proxy o
  -> Value k a
  -> PipeFunTy as o
applyPipeFun' f _ _ = \case
  VPoint x -> f x
  VList  x -> f x
  VSet   x -> f x
  VTree  x -> f x
  VDag   x -> f x
  VGraph x -> f x

applyPipeFun
  :: (Repr k a -> r) -> Value k a
  -> r
applyPipeFun f = \case
  VPoint x -> f x
  VList  x -> f x
  VSet   x -> f x
  VTree  x -> f x
  VDag   x -> f x
  VGraph x -> f x
