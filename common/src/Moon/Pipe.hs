{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}
module Moon.Pipe
  ( Pipe(..)
  , Result
  , pattern PGen
  , pattern PLink
  , pattern PAny
  , pipeName
  , runPipe
  , Pipes
  -- *
  , gen
  , Moon.Pipe.link
  , apply
  , compose
  )
where

import           Control.Monad
import           Data.Dynamic
import qualified Algebra.Graph                    as G
import qualified Data.Kind                        as K
import           Data.Map                           (Map)
import           Data.Proxy
import           Data.Text                          (Text, pack)
import qualified Type.Reflection                  as R
import           Type.Reflection--                    (pattern App)
import           Unsafe.Coerce                      (unsafeCoerce)

import           Moon.Face

--------------------------------------------------------------------------------
-- | Pipe: a concrete, runnable 'Def'-inition.
data Pipe = forall (k :: Con) (a :: *)
  . (GroundContext a, Typeable k)
  => Pipe
  { pDef :: Def
  , pTo  :: Tag k a
  , pDyn :: Dynamic -- ^ A wrapped 'PipeFun'.
  }

-- | Result of running a pipe.
type Result a = IO (Either Text a)

pattern PGen  :: PipeName -> Struct -> Dynamic ->         Type -> Pipe
pattern PGen  n st dy    so <- Pipe (Def n (Gen     so) st) _ dy

pattern PLink :: PipeName -> Struct -> Dynamic -> Type -> Type -> Pipe
pattern PLink n st dy si so <- Pipe (Def n (Link si so) st) _ dy

pattern PAny  :: PipeName -> Struct -> Dynamic ->         Type -> Pipe
pattern PAny  n st dy    so <- Pipe (Def n (sOut -> so) st) _ dy

instance Show Pipe where
  show (pDef -> d) = "Pipe "<>show (dPipeName d)<>" "<>show (dSig d)

pipeName :: Pipe -> PipeName
pipeName = dPipeName . pDef

data PipeFun (ka :: Con) a (kb :: Con) b where
  FOutput :: forall kb b
           . (Typeable kb, GroundContext b)
          => Tag kb b ->               Result (Repr kb b)  -> PipeFun Point () kb b
  FLink   :: forall ka a kb b
           . (Typeable ka, GroundContext a, Typeable kb, GroundContext b)
          => Tag kb b -> (Repr ka a -> Result (Repr kb b)) -> PipeFun    ka  a kb b

instance Show (PipeFun ka a kb b) where
  show FOutput{} = "FOutput Point () "<>show (typeRep @kb)<>" "<>show (typeRep @b)
  show FLink{}   = "FLink "<>show (typeRep @a)<>" "<>show (typeRep @b)

runPipe' :: forall k a
          . (Typeable k, GroundContext a)
         => Tag (k :: Con) (a :: *)
         -> Dynamic
         -> Result SomeValue
runPipe' _tagTo dyn =
  case fromDynamic dyn :: Maybe (PipeFun Point () k a) of
    Nothing -> pure . Left $ "Incomplete pipe: " <> pack ""
    Just (FOutput tagTo pipeFun) -> do
      (SomeValue . SomeKindValue . mkValue tagTo <$>) <$> pipeFun

runPipe :: Pipe -> Result SomeValue
runPipe (Pipe _ tag dyn) = runPipe' tag dyn

--------------------------------------------------------------------------------
-- | Pipes: executable version of Scope.
type Pipes = Map PipeName Pipe

--------------------------------------------------------------------------------
-- | Pipe constructors
gen
  :: forall kt tt
  .  (Typeable (Repr kt tt), Typeable kt, GroundContext tt)
  => PipeName
  -> Tag kt tt
  -> Result (Repr kt tt)
  -> Pipe
gen name tagTo mv
                = Pipe def tagTo dyn
  where def     = Def name sig struct
        ty      = tagType tagTo
        sig     = Gen ty
        struct  = Struct graph
        graph   = G.vertex ty
        dyn     = Dynamic R.typeRep pipeFun
        pipeFun = FOutput tagTo mv :: PipeFun Point () kt tt

link
  :: forall kf tf kt tt
  . ( Typeable (Repr kf tf)
    , Typeable (Repr kt tt)
    , Typeable kf
    , GroundContext tf
    , Typeable kt
    , GroundContext tt)
  => PipeName
  -> Tag kf tf
  -> Tag kt tt
  -> (Repr kf tf -> Result (Repr kt tt))
  -> Pipe
link name tagFrom tagTo mf
                = Pipe def tagTo dyn
  where def     = Def name sig struct
        tyFrom  = tagType tagFrom
        tyTo    = tagType tagTo
        sig     = Link tyFrom tyTo
        struct  = Struct graph
        graph   = G.connect (G.vertex tyFrom) (G.vertex tyTo)
        dyn     = Dynamic R.typeRep pipeFun
        pipeFun = FLink tagTo mf :: PipeFun kf tf kt tt

apply :: [SomeValue] -> Pipe -> Either Text Pipe
apply [] p
  =   Right p
apply _ p@PGen{}
  =   Left $ "Cannot apply values to a fully saturated pipe: " <> pack (show . dPipeName $ pDef p)
apply (SomeValue (SomeKindValue (v :: Value ka a) :: SomeKindValue a):xs)
      (PLink rpn rs
        (Dynamic (App (App (App (App (Con _tc') kb') b') _kc') _c') mf)
        _rfrom _rto)
  | Just HRefl <- typeRep @ka `eqTypeRep` kb'
  , Just HRefl <- typeRep  @a `eqTypeRep`  b'
  = join $ apply xs <$> doApply v (unsafeCoerce mf) rpn rs

doApply
  :: forall  ka a kb b ra rb
   . ( ra ~ Repr ka a
     , rb ~ Repr kb b
     , GroundContext a)
  => Value       ka a
  -> PipeFun     ka a kb b -> PipeName -> Struct
  -> Either Text Pipe
doApply v
        (FLink tagTo (f :: ra -> Result rb))
        (PipeName rn)
        (Struct rg)
  = Right $ Pipe def tagTo dyn
  where def     = Def (PipeName $ "app-"<>rn) sig struct
        ty      = pxType (Proxy @kb) (Proxy @b)
        sig     = Gen ty
        struct  = Struct graph
        graph   = rg -- XXX ???
        dyn     = Dynamic (typeRep :: TypeRep (PipeFun Point () kb b)) pipeFun
        pipeFun = FOutput tagTo (applyPipe f v) :: PipeFun Point () kb b
doApply v f _ _ = Left $ "doApply: Value+PipeFun, but lv="<>pack (show v)<>" rfun="<>pack (show f)

-- XXX: ExceptT
applyPipe :: forall k a ra rb
           . ( ra ~ Repr k a
             , GroundContext a)
          => (ra -> Result rb) -> Value k a -> Result rb
applyPipe f = \case
  VPoint x -> f x
  VList  x -> f x
  VSet   x -> f x
  VTree  x -> f x
  VDag   x -> f x
  VGraph x -> f x

doBind
  :: forall  ka a kb b kc c rb rc
   . ( rb ~ Repr kb b
     , rc ~ Repr kc c)
  => PipeFun ka a kb b      -> PipeName -> Struct
  -> PipeFun      kb b kc c -> PipeName -> Struct
  -> Either Text Pipe
doBind (FOutput _tagFrom (v :: Result rb))  (PipeName ln) (Struct lg)
       (FLink tagTo (f :: rb -> Result rc)) (PipeName rn) (Struct rg)
  = Right $ Pipe def tagTo dyn
  where def     = Def (PipeName $ ln<>"<>"<>rn) sig struct
        ty      = pxType (Proxy @kc) (Proxy @c)
        sig     = Gen ty
        struct  = Struct graph
        graph   = G.overlay lg rg
        dyn     = Dynamic (typeRep :: TypeRep (PipeFun Point () kc c)) pipeFun
        pipeFun = FOutput tagTo (bindPipes v f) :: PipeFun Point () kc c
doBind lfun _ _ rfun _ _ = Left $ "doBind: PipeFuns, but lfun="<>pack (show lfun)<>" rfun="<>pack (show rfun)

-- XXX: ExceptT
bindPipes :: Result a -> (a -> Result b) -> Result b
bindPipes v f = do
  r <- v
  case r of
    Left e  -> pure $ Left e
    Right x -> f x

compose :: Pipe -> Pipe -> Either Text Pipe
compose PGen{} PGen{}
  =   Left $ "Cannot compose two Gen pipes."
compose (PAny  lpn ls
         (Dynamic l@(App (App (App (App (Con tc ) _ka ) _a ) kb)  b ) mv)
         lto)
        (PLink rpn rs
         (Dynamic r@(App (App (App (App (Con tc') kb') b') _kc') _c') mf)
         rfrom _rto)
  | lto /= rfrom =
      Left $ "Left pipe output doesn't match right pipe input: Left out "<>pack (show lto)<>", Right in "<>pack (show rfrom)<>"."
  -- Only doing primitive composition
  | tc == typeRepTyCon (typeRep @PipeFun)
  , tc == tc'
  , Just HRefl <- kb `eqTypeRep` kb'
  , Just HRefl <-  b `eqTypeRep`  b'
  , Just HRefl <- typeRep @K.Type `eqTypeRep` typeRepKind kb
  , Just HRefl <- typeRep @K.Type `eqTypeRep` typeRepKind  b
  = doBind (unsafeCoerce mv) lpn ls (unsafeCoerce mf) rpn rs
  | otherwise
  = Left $ "compose: (App (App (Con ..)..)..), but l="<>pack (show l)<>" r="<>pack (show r)
compose (PAny _ _ l _) (PLink _ _ r _ _)
  = Left $ "compose: not (App (App (Con ..)..)..), left="<>pack (show l)<>" right="<>pack (show r)
