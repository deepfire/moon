{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ViewPatterns #-}
module Moon.Pipe
  ( Pipe
  , pipeTy
  , defOutput
  , defLink
  , compose
  )
where

import           Data.Dynamic
import           Data.Kind (Type)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Proxy
import           Type.Reflection
import           Unsafe.Coerce

import           Moon.Face


data Pipe = Pipe
  { pDyn  :: Dynamic
  , pKey  :: PipeTy
  }

pipeTy :: Pipe -> PipeTy
pipeTy = pKey

data PipeFun (ka :: Kind) a (kb :: Kind) b where
  FOutput :: forall kb b
           . (Typeable kb, Typeable b)                          =>               IO (Repr kb b)  -> PipeFun Point () kb b
  FLink   :: forall ka a kb b
           . (Typeable ka, Typeable a, Typeable kb, Typeable b) => (Repr ka a -> IO (Repr kb b)) -> PipeFun    ka  a kb b

defOutput
  :: forall k a. (Typeable k, Typeable a)
  => Tag k a -> IO (Repr k a) -> Pipe
defOutput t x =
  Pipe (toDyn $ FOutput @k @a x)
       (Output $ tagMeta t)

defLink
  :: forall ka a kb b. (Typeable ka, Typeable a, Typeable kb, Typeable b)
  => Tag ka a -> Tag kb b
  -> (Repr ka a -> IO (Repr kb b))
  -> Pipe
defLink ta tb x =
  Pipe (toDyn $ FLink @ka @a @kb @b x)
       (Link (tagMeta ta) (tagMeta tb))

compose
  :: Pipe -> Pipe
  -> Either String Pipe
compose (Pipe _ Output{}) (Pipe _ Output{})
  =   Left $ "Cannot compose two Output pipes."
compose (Pipe l (ptOut -> lto)) (Pipe r (ptIn -> rfrom))
  | lto /= rfrom =
      Left $ "Left pipe output doesn't match right pipe input: Left out "<>show lto<>", Right in "<>show rfrom<>"."
  | otherwise = dynBind l r 

-- * Non-user-serviceable.
--
instance Show (PipeFun ka a kb b) where
  show FOutput{} = "FOutput Point () "<>show (typeRep @kb)<>" "<>show (typeRep @b)
  show FLink{}   = "FLink "<>show (typeRep @a)<>" "<>show (typeRep @b)

-- PipeFun a b -- PipeFun b c
-- (a::(m a)) `dynBind` (b::a -> m b) = (a >>= b)::m b
dynBind :: Dynamic -> Dynamic -> Either String Pipe
dynBind (Dynamic l@(App (App (App (App (Con tc ) ka ) a ) kb)  b ) mv)
        (Dynamic r@(App (App (App (App (Con tc') kb') b') kc') c') mf)
  -- Only doing primitive composition
  | tc == typeRepTyCon (typeRep @PipeFun)
  , tc == tc'
  , Just HRefl <- kb `eqTypeRep` kb'
  , Just HRefl <-  b `eqTypeRep`  b'
  , Just HRefl <- typeRep @Kind `eqTypeRep` typeRepKind kb
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind  b
  = let doBind :: forall  ka a kb b kc c rb rc
                . (rb ~ Repr kb b, rc ~ Repr kc c)
               => PipeFun ka a kb b
               -> PipeFun      kb b kc c
               -> Either String Pipe
        doBind (FOutput (v :: IO rb)) (FLink (f :: rb -> IO rc))
          = Right $ Pipe (Dynamic (typeRep :: TypeRep (PipeFun Point () kc c)) (FOutput (v >>= f)))
                         (Output $ meta (Proxy @kc) (Proxy @c))
        doBind lfun rfun = Left $ "doBind: PipeFuns, but lfun="<>show lfun<>" rfun="<>show rfun
    in doBind (unsafeCoerce mv) (unsafeCoerce mf)
  | otherwise
  = Left $ "doBind: (App (App (Con ..)..)..), but l="<>show l<>" r="<>show r
dynBind (Dynamic l _) (Dynamic r _)
  = Left $ "doBind: not (App (App (Con ..)..)..), left="<>show l<>" right="<>show r
