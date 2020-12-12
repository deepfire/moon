module Debug.Reflex where

import Control.Exception
import Debug.TraceErr (traceErr, traceHandle)
import Data.String (IsString(..))
import Data.Functor ((<&>))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Reflex
import Data.IORef qualified                       as IO
import System.IO qualified                        as IO
import System.IO.Unsafe qualified                 as IO


traceErrDynWith :: Reflex t => (a -> String) -> Dynamic t a -> Dynamic t a
traceErrDynWith f d =
  unsafeBuildDynamic getV0 (traceEventWith f $ updated d)
 where
   getV0 = do
     x <- sample $ current d
     traceErr (f x) $ return x

traceErrDynEvWith :: Reflex t => (a -> String) -> Dynamic t a -> Dynamic t a
traceErrDynEvWith f d =
  unsafeBuildDynamic (sample $ current d) (traceEventWith f $ updated d)

traceErrEventWith :: Reflex t => (a -> String) -> Event t a -> Event t a
traceErrEventWith f =
  push $
    \x ->
      traceErr (f x) . pure $ Just x

trdyn :: Reflex t => (a -> String) -> Dynamic t a -> Dynamic t a
trdyn = traceErrDynWith
{-# INLINE trdyn #-}

trdev :: Reflex t => (a -> String) -> Dynamic t a -> Dynamic t a
trdev = traceErrDynEvWith
{-# INLINE trdev #-}

trev :: Reflex t => (a -> String) -> Event t a -> Event t a
trev = traceErrEventWith
{-# INLINE trev #-}

trdyns :: Reflex t => String -> Dynamic t a -> Dynamic t a
trdyns = traceErrDynWith . const
{-# INLINE trdyns #-}

trdevs :: Reflex t => String -> Dynamic t a -> Dynamic t a
trdevs = traceErrDynEvWith . const
{-# INLINE trdevs #-}

trevs :: Reflex t => String -> Event t a -> Event t a
trevs = traceErrEventWith . const
{-# INLINE trevs #-}

trd :: ((a -> String) -> f a -> f a) -> String -> (a -> String) -> f a -> f a
trd f d showf = f ((d <>) . (": " <>) . showf)
{-# INLINE trd #-}

trdynd :: (Reflex t) => String -> (a -> String) -> Dynamic t a -> Dynamic t a
trdynd = trd traceErrDynWith
{-# INLINE trdynd #-}

trdevd :: (Reflex t) => String -> (a -> String) -> Dynamic t a -> Dynamic t a
trdevd = trd traceErrDynEvWith
{-# INLINE trdevd #-}

trevd :: (Reflex t) => String -> (a -> String) -> Event t a -> Event t a
trevd = trd traceErrEventWith
{-# INLINE trevd #-}

--------------------------------------------------------------------------------
-- * .dot tracing
--
{-# NOINLINE _dotTraceHandleRef #-}
_dotTraceHandleRef :: IO.IORef (IO.Handle)
_dotTraceHandleRef = IO.unsafePerformIO . IO.newIORef $
  error ".dot trace handle not yet initialised."

{-# NOINLINE _dotTraceHandle #-}
_dotTraceHandle :: IO.Handle
_dotTraceHandle = IO.unsafePerformIO $ IO.readIORef _dotTraceHandleRef

setupDotTracing :: FilePath -> IO ()
setupDotTracing fp = do
  hnd <- IO.openFile fp IO.WriteMode
  IO.hPutStrLn hnd "digraph trace {"
  IO.writeIORef _dotTraceHandleRef hnd
{-# NOINLINE setupDotTracing #-}

closeDotTrace :: IO ()
closeDotTrace = do
  IO.hPutStrLn _dotTraceHandle "}"
  IO.hClose _dotTraceHandle
{-# NOINLINE closeDotTrace #-}

--------------------------------------------------------------------------------
-- * Ev/Dyn -> String -> .dot raw
--
withDotTracing :: FilePath -> IO () -> IO ()
withDotTracing fp body = do
  setupDotTracing fp
  catches (body >> closeDotTrace)
    [ Handler $ \SomeException{} -> closeDotTrace
    ]

traceDotEventWith :: Reflex t => (a -> String) -> Event t a -> Event t a
traceDotEventWith f =
  push $
    \x ->
      traceHandle _dotTraceHandle (f x) . pure $ Just x

traceDotDynEvWith :: Reflex t => (a -> String) -> Dynamic t a -> Dynamic t a
traceDotDynEvWith f d =
  unsafeBuildDynamic (sample $ current d) (traceDotEventWith f $ updated d)

--------------------------------------------------------------------------------
-- * src/dst + f a -> .dot edge
--
newtype To   = To   String deriving (IsString)
newtype From = From String deriving (IsString)

trdot :: ((a -> String) -> f a -> f a) -> To -> From -> f a -> f a
trdot f (To to) (From from) =
  f (const $ from <> " -> " <> to <> ";")
{-# INLINE trdot #-}

trdotlab :: ((a -> String) -> f a -> f a) -> To -> From -> (a -> String) -> f a -> f a
trdotlab f (To to) (From from) showf =
  f (((from <> " -> " <> to <> " [label=\"") <>)
     . (<> "\"];")
     . showf)
{-# INLINE trdotlab #-}

--------------------------------------------------------------------------------
-- * src/dst + Ev/Dyn a -> .dot edge
--
trdotev :: Reflex t => To -> From -> Event t a -> Event t a
trdotev = trdot traceDotEventWith

trdotdev :: Reflex t => To -> From -> Dynamic t a -> Dynamic t a
trdotdev = trdot traceDotDynEvWith

trdotlabev :: Reflex t => To -> From -> (a -> String) -> Event t a -> Event t a
trdotlabev = trdotlab traceDotEventWith
{-# INLINE trdotlabev #-}

trdotlabdev :: Reflex t => To -> From -> (a -> String) -> Dynamic t a -> Dynamic t a
trdotlabdev = trdotlab traceDotDynEvWith
{-# INLINE trdotlabdev #-}

trdotevN,      trdotdevN,  trdotlabevN,  trdotlabdevN,  toN,  fromN :: Exp
[trdotevN,     trdotdevN,  trdotlabevN,  trdotlabdevN,  toN,  fromN] =
  (["trdotev", "trdotdev", "trdotlabev", "trdotlabdev"]
   <&> VarE . mkName)
  <>
  (["To","From"]
   <&> ConE . mkName)

dot' :: Exp -> To -> From -> Q Exp
dot' tracer (To to) (From from) = pure $
  AppE (AppE tracer (AppE toN   (LitE . StringL $ to)))
                    (AppE fromN (LitE . StringL $ from))

dot :: Exp -> To -> Name -> Q Exp
dot tracer to x@(Name (OccName from) _) =
  AppE <$> dot' tracer to (From from) <*> pure (VarE x)

ev' :: To -> From -> Q Exp
ev' = dot' trdotevN

ev :: To -> Name -> Q Exp
ev = dot trdotevN

dev :: To -> Name -> Q Exp
dev = dot trdotdevN

evl'' :: To -> From -> Q Exp -> Q Exp
evl'' to from showExp =
  AppE <$> dot' trdotlabevN to from
       <*> showExp

evl' :: To -> Name -> Q Exp -> Q Exp
evl' to (Name (OccName from) _) showExp =
  AppE <$> dot' trdotlabevN to (From from)
       <*> showExp

evl :: To -> Name -> Q Exp -> Q Exp
evl to from showExp' =
  AppE <$> evl' to from showExp'
       <*> pure (VarE from)

devl' :: To -> Name -> Q Exp -> Q Exp
devl' to (Name (OccName from) _) showExp =
  AppE <$> dot' trdotlabdevN to (From from)
       <*> showExp

devl :: To -> Name -> Q Exp -> Q Exp
devl to from showExp' =
  AppE <$> devl' to from showExp'
       <*> pure (VarE from)

showQ :: Show a => a -> String
showQ = fmap (\case { '"' -> '\''; x -> x; }) . show
{-# INLINE showQ #-}
