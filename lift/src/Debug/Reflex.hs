module Debug.Reflex
where

import Debug.TraceErr (traceErr)
import Reflex


traceErrDynWith :: Reflex t => (a -> String) -> Dynamic t a -> Dynamic t a
traceErrDynWith f d =
  let e' = traceEventWith f $ updated d
      getV0 = do
        x <- sample $ current d
        traceErr (f x) $ return x
  in unsafeBuildDynamic getV0 e'

traceErrEventWith :: Reflex t => (a -> String) -> Event t a -> Event t a
traceErrEventWith f = push $ \x -> traceErr (f x) $ return $ Just x

trdyn :: Reflex t => (a -> String) -> Dynamic t a -> Dynamic t a
trdyn = traceErrDynWith

trev :: Reflex t => (a -> String) -> Event t a -> Event t a
trev = traceErrEventWith
