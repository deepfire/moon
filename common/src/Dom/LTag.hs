{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Dom.LTag (module Dom.LTag) where

import Codec.CBOR.Decoding              qualified as CBOR
import Codec.CBOR.Encoding              qualified as CBOR
import Codec.Serialise
import Data.IORef                       qualified as IO
import Data.Kind
import Data.Proxy
import Reflex
import System.IO.Unsafe                 qualified as IO
import Unsafe.Coerce                    qualified as Unsafe
import Type.Reflection

import Dom.Reflex


data Liveness
  = Now
  | forall (t :: *) (m :: *). Live t m
  -- deriving (Generic, Eq, Ord)

data LTag (l :: Liveness) where
  LNow  :: LTag Now
  LLive :: forall (t :: *) (m :: * -> *).
    (MonadReflex t m, Typeable t)
    => TypeRep t
    -> TypeRep m
    -> LTag (Live t m)

type LiveConstr l = (Typeable l, LiveConstr' l)

type family LiveConstr' (l :: Liveness) :: Constraint where
  LiveConstr' Now        = ()
  LiveConstr' (Live t m) = (Typeable t, MonadReflex t m)

data SomeLTag where
  SomeLTag
    :: (--ReifyLTag l,
        Typeable l, LiveConstr l)
    => LTag (l :: Liveness) -> SomeLTag

withLivenessKindRep ::
  forall (l :: Liveness) a
  . Typeable l
  => LTag l -> (TypeRep l -> a) -> a
withLivenessKindRep _l f = f (typeRep @l)

instance Serialise SomeLTag where
  encode = \(SomeLTag t) -> case t of
    LLive trT trM -> CBOR.encodeWord 1
                  <> encode (SomeTypeRep trT)
                  <> encode (SomeTypeRep trM)
    LNow          -> CBOR.encodeWord 2
  decode = do
    cid <- CBOR.decodeWord
    case cid of
      1 -> do
        SomeTypeRep (_ :: TypeRep t') <- decode
        SomeTypeRep (_ :: TypeRep m') <- decode
        pure someLTagLive
      2 -> pure $ SomeLTag LNow
      _ -> fail $ "invalid SomeLTag encoding: tag="<>show cid

{-# NOINLINE _someLTagLiveRef #-}
_someLTagLiveRef :: IO.IORef SomeLTag
_someLTagLiveRef = IO.unsafePerformIO . IO.newIORef $
  error "someLTagLiveRef not yet initialised."

{-# NOINLINE someLTagLive #-}
someLTagLive :: SomeLTag
someLTagLive = IO.unsafePerformIO $ IO.readIORef _someLTagLiveRef

{-# NOINLINE setupSomeLTagLive #-}
setupSomeLTagLive :: SomeLTag -> IO ()
setupSomeLTagLive = IO.writeIORef _someLTagLiveRef

-- class ReifyLTag (l :: Liveness) where
--   reifyLTag :: Proxy l -> LTag l

-- instance ReifyLTag Now where
--   reifyLTag = const LNow
-- instance (LiveConstr (Live t m)) => ReifyLTag (Live (t :: *) (m :: *)) where
--   reifyLTag =
--     \case
--       (Proxy :: Proxy (Live t)) ->
--         LLive (typeRep @t)
