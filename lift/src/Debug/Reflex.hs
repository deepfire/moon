module Debug.Reflex where

import Debug.TraceErr (traceErr)
import Reflex


traceErrDynWith :: Reflex t => (a -> String) -> Dynamic t a -> Dynamic t a
traceErrDynWith f d =
  unsafeBuildDynamic getV0 (traceEventWith f $ updated d)
 where
   getV0 = do
     x <- sample $ current d
     traceErr (f x) $ return x

traceErrEventWith :: Reflex t => (a -> String) -> Event t a -> Event t a
traceErrEventWith f =
  push $
    \x ->
      traceErr (f x) . pure $ Just x

trdyn :: Reflex t => (a -> String) -> Dynamic t a -> Dynamic t a
trdyn = traceErrDynWith

trev :: Reflex t => (a -> String) -> Event t a -> Event t a
trev = traceErrEventWith

trdyns :: Reflex t => String -> Dynamic t a -> Dynamic t a
trdyns = traceErrDynWith . const

trevs :: Reflex t => String -> Event t a -> Event t a
trevs = traceErrEventWith . const
