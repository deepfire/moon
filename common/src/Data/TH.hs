module Data.TH (module Data.TH) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax       qualified as TH

import Debug.Trace

--------------------------------------------------------------------------------
-- * Template Haskell tools
--
thAppChain :: TH.Type -> [TH.Type]
thAppChain = go []
  where
    go acc (TH.AppT f x) = go (x:acc) f
    go acc t = reverse (t:acc)

thAppFromChain :: [TH.Type] -> TH.Type
thAppFromChain (reverse -> (x:xs)) = go x xs
  where
    go acc [] = acc
    go acc (y:ys) = go (TH.AppT acc y) ys
--{-# COMPLETE thAppFromChain #-}

thAppHead :: TH.Type -> TH.Type
thAppHead = \case
  TH.AppT f _ -> thAppHead f
  x -> x

thAppTailIf :: (TH.Type -> Bool) -> TH.Type -> TH.Type
thAppTailIf test = \case
  TH.AppT f@(TH.AppT{}) x -> TH.AppT (thAppTailIf test f) x
  TH.AppT (test -> True) x -> x
  x -> x

thNameIsInstance :: TH.Name -> TH.Type -> Q Bool
thNameIsInstance instN t = isInstance instN [t]

thNameIsInstanceSpine :: TH.Name -> TH.Type -> Q Bool
thNameIsInstanceSpine instN = go True
 where
   go top ty =
     case ty of
       TH.ConT{}    -> if top then nodeIsInstance else pure True
       TH.AppT _f x -> (&&)
                         <$> nodeIsInstance
                         <*> go False x
       TH.TupleT _n -> nodeIsInstance
       x -> pure (error $ "thNameIsInstanceSpine: unhandled case: " <> show x)
    where
      nodeIsInstance :: Q Bool
      nodeIsInstance =
        (\x -> trace ("isInstance " <> show instN <> " " <> show ty <> ": " <> show x) x) <$>
        isInstance instN [ty]

thNameIsInstanceDeep :: TH.Name -> TH.Type -> Q Bool
thNameIsInstanceDeep instN ty = case ty of
  TH.ConT{}    -> isInstance instN [ty]
  TH.AppT _f x -> (&&)
                    <$> isInstance instN [ty]
                    <*> thNameIsInstanceDeep instN x
  TH.TupleT _n -> isInstance instN [ty]
  x -> pure (error $ "thNameIsInstanceDeep: unhandled case: " <> show x)
