{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module ExpMod where

import           Data.Dynamic
import           Data.Kind (Type)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Proxy
import           Type.Reflection
import           Unsafe.Coerce


data PipeKey
  = KeyFull -- | IO a
    { pkTo   :: SomeTypeRep
    }
  | KeyLink -- | b → IO c
    { pkTo   :: SomeTypeRep
    , pkFrom :: SomeTypeRep
    } deriving (Eq, Ord, Show)

data PipeFun a b where
  Full ::              Typeable b  =>       IO b  -> PipeFun () b
  Link :: (Typeable a, Typeable b) => (a -> IO b) -> PipeFun a  b

instance Show (PipeFun a b) where
  show Full{} = "Full () "<>show (typeRep @b)
  show Link{} = "Link "<>show (typeRep @a)<>" "<>show (typeRep @b)

data Pipe = Pipe
  { pDyn :: Dynamic
  , pKey :: PipeKey
  }

data Pipes = Pipes (Map PipeKey Pipe)

mkFull :: forall a. Typeable a => IO a -> Pipe
mkFull x = Pipe (toDyn $ Full     @a x) (KeyFull (someTypeRep $ Proxy @a))

mkLink :: forall a b. (Typeable a, Typeable b) => (a -> IO b) -> Pipe
mkLink x = Pipe (toDyn $ Link @a  @b x) (KeyLink (someTypeRep $ Proxy @b) (someTypeRep $ Proxy @a))

-- (a::(m a)) `dynBind` (b::a -> m b) = (a >>= b)::m b
dynBind :: Dynamic -> Dynamic -> Either String Pipe
dynBind (Dynamic l@(App (App (Con tc)  t1)  t2)  mv)
        (Dynamic r@(App (App (Con tc') t1') t2') mf)
  | tc == typeRepTyCon (typeRep @PipeFun)
  , tc == tc'
  , Just HRefl <- t2 `eqTypeRep` t1'
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind t1
  = let doBind :: PipeFun a b -> PipeFun b c -> Either String Pipe
        doBind (Full (v :: IO b)) (Link (f :: b -> IO c))
          = Right $ Pipe (Dynamic typeRep (Full (v >>= f)))
            (KeyFull $ someTypeRep $ Proxy @c)
        doBind ldyn rdyn = Left $ "doBind: PipeFuns, but ldyn="<>show ldyn<>" rdyn="<>show rdyn
    in doBind (unsafeCoerce mv) (unsafeCoerce mf)
  | otherwise
  = Left $ "doBind: (App (App (Con ..)..)..), but l="<>show l<>" r="<>show r
dynBind (Dynamic l _) (Dynamic r _)
  = Left $ "doBind: not (App (App (Con ..)..)..), left="<>show l<>" right="<>show r

compose :: Pipe -> Pipe -> Either String Pipe
compose (Pipe _ KeyFull{}) (Pipe _ KeyFull{}) =
  Left "Cannot combine two Full pipes."
compose (Pipe ldyn (pkTo -> lto)) (Pipe rdyn (pkFrom -> rfrom))
  | lto /= rfrom =
      Left $ "Left pipe output doesn't match right pipe input: Left out "<>show lto<>", Right in "<>show rfrom<>"."
  | otherwise = dynBind ldyn rdyn 

-- typeOf :: Typeable a => a -> TypeRep a Source #

-- testEquality :: TypeRep a -> TypeRep b -> Maybe (a :~: b) Source #
-- show :: TypeRep a -> String Source #
-- typeRep @(Maybe Int) === App (typeRep @Maybe) (typeRep @Int)
-- (==) :: TypeRep a -> TypeRep a -> Bool Source #
-- typeRepTyCon :: TypeRep a -> TyCon Source #
-- typeRepKind :: TypeRep (a :: k) -> TypeRep k Source #

-- splitApps :: TypeRep a -> (TyCon, [SomeTypeRep]) Source #

-- (==) :: SomeTypeRep -> SomeTypeRep -> Bool Source #
-- someTypeRep :: forall proxy a. Typeable a => proxy a -> SomeTypeRep Source #
-- someTypeRepTyCon :: SomeTypeRep -> TyCon Source #

-- * dynTypeRep :: Dynamic -> SomeTypeRep

-- * toDyn :: Typeable a => a -> Dynamic
-- * fromDyn :: Typeable a => Dynamic -> a -> a
-- * fromDynamic :: forall a. Typeable a => Dynamic -> Maybe a
-- * dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
-- * dynApp :: Dynamic -> Dynamic -> Dynamic

-- * data TypeRep (a :: k)
-- * typeOf :: Typeable a => a -> TypeRep a
-- * pattern App :: forall k2 (t :: k2). () => forall k1 (a :: k1 -> k2) (b :: k1). t ~ a b => TypeRep a -> TypeRep b -> TypeRep t
-- * pattern Con :: forall k (a :: k). () => IsApplication a ~ "" => TyCon -> TypeRep a
-- * pattern Con' :: forall k (a :: k). () => IsApplication a ~ "" => TyCon -> [SomeTypeRep] -> TypeRep a
-- * pattern Fun :: forall k (fun :: k). () => forall (r1 :: RuntimeRep) (r2 :: RuntimeRep) (arg :: TYPE r1) (res :: TYPE r2). (k ~ Type, fun ~~ (arg -> res)) => TypeRep
--  arg -> TypeRep res -> TypeRep fun
