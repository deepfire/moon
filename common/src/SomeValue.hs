{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module SomeValue
  ( readSomeValue
  , stripValue
  -- , stripSomeValue
  , unitSomeValue
  , someValueSomeCTag
  , someValueSomeTypeRep
  , someValueKindedSomeTypeRep
  , someValueSomeType
  , withSomeValue
  , parseSomeValue
  --
  , module Ground.Table
  )
where

import qualified Data.Set.Monad                   as Set
import           Text.Read                           (ReadPrec)
import qualified Type.Reflection                  as R

import Basis
import Data.Parsing

import Type
import SomeType
import Ground.Table

parseSomeValue :: Parser SomeValue
parseSomeValue =
  (SomeValue TPoint . SomeValueKinded VText . mkValue' (Proxy @Text) TPoint <$> stringLiteral)
  <|>
  (SomeValue TPoint . SomeValueKinded VInteger . mkValue' (Proxy @Integer) TPoint <$> integer)
  <|>
  (SomeValue TPoint . SomeValueKinded VInteger . mkValue' (Proxy @Integer) TPoint <$> hexadecimal)
  <|>
  (SomeValue TPoint . SomeValueKinded VDouble . mkValue' (Proxy @Double) TPoint <$> double)
  <|>
  braces   (parseSV . reifyCTag $ Proxy @Point)
  <|>
  brackets (parseSV . reifyCTag $ Proxy @List)
 where
   parseSV :: CTag c -> Parser SomeValue
   parseSV ctag = do
     TyDict a :: TyDict Ground <- parser
     case ctag of
       TPoint -> do
         v :: a <- parser
         let vtag = reifyVTag $ Proxy @a
         pure $ SomeValue ctag $ SomeValueKinded vtag $ mkValue' a ctag v
         -- pure $ mkSomeValue ctag vtag v
       TList -> do
         v :: [a] <- commaSep parser
         let vtag = reifyVTag $ Proxy @a
         pure $ SomeValue ctag $ SomeValueKinded vtag $ mkValue' a ctag v
         -- pure $ mkSomeValue ctag (reifyVTag $ Proxy @a) v
       TSet -> do
         v :: [a] <- commaSep parser
         let vtag = reifyVTag $ Proxy @a
         pure $ SomeValue ctag $ SomeValueKinded vtag $ mkValue' a ctag (Set.fromList v)
         -- pure $ mkSomeValue ctag (reifyVTag $ Proxy @a) (Set.fromList v)
       _ -> trace (printf "No parser for structures outside of Point/List/Set.")
                  (fail "")

readSomeValue :: forall (c :: Con). CTag c -> TyDict Ground -> ReadPrec SomeValue
readSomeValue ctag (TyDict (a :: Proxy a)) =
  case ctag of
    TPoint -> do
      v :: a <- readPrec
      pure $ mkSomeValue ctag (reifyVTag $ Proxy @a) v
    TList -> do
      v :: [a] <- readListPrec
      pure $ mkSomeValue ctag (reifyVTag $ Proxy @a) v
    TSet -> do
      v :: [a] <- readListPrec
      pure $ mkSomeValue ctag (reifyVTag $ Proxy @a) (Set.fromList v)
    _ -> trace (printf "No parser for structures outside of Point/List/Set.")
               (fail "")

unitSomeValue :: SomeValue
unitSomeValue = SomeValue TPoint $ SomeValueKinded VUnit (VPoint ())

stripValue ::
  forall (c :: Con) a
  .  Value c a
  -> Repr c a
stripValue = \case
  VPoint x -> x
  VList  x -> x
  VSet   x -> x
  VTree  x -> x
  VDag   x -> x
  VGraph x -> x

-- stripSomeValue ::
--   forall a c
--    . (Typeable a, Typeable c)
--   => CTag c
--   -> Proxy a
--   -> SomeValue
--   -> Maybe (Repr c a)
-- stripSomeValue _ _ (SomeValue (_ :: CTag c') (SomeValueKinded (r :: Value c' a'))) =
--   let exptr = typeRep @a
--       svtr  = typeRep @a'
--       expk  = typeRep @c
--       svk   = typeRep @c'
--   in case (,) (svtr `R.eqTypeRep` exptr)
--               (svk  `R.eqTypeRep` expk) of
--     (Just R.HRefl, Just R.HRefl) -> Just $ stripValue r
--     _ -> Nothing

withSomeValue
  :: forall a c b
   . (Typeable a, Typeable c)
  => CTag c
  -> Proxy a
  -> SomeValue
  -> (Value c a -> b)
  -> Either Text b
withSomeValue _ _ (SomeValue (_ :: CTag c') (SomeValueKinded _ (r :: Value c' a'))) f =
  let exptr = typeRep @a
      svtr  = typeRep @a'
      expk  = typeRep @c
      svk   = typeRep @c'
  in case (,) (svtr `R.eqTypeRep` exptr)
              (svk  `R.eqTypeRep` expk) of
    (Just R.HRefl, Just R.HRefl) -> Right $ f r
    _ -> Left . pack $ printf "withSomeValue: expected %s/%s, got %s/%s"
                (show exptr) (show expk) (show svtr) (show svk)

someValueSomeCTag :: SomeValue -> SomeCTag
someValueSomeCTag (SomeValue t _) = SomeCTag t

someValueSomeTypeRep :: SomeValue -> R.SomeTypeRep
someValueSomeTypeRep (SomeValue _ svk) = someValueKindedSomeTypeRep svk

someValueKindedSomeTypeRep :: SomeValueKinded c -> R.SomeTypeRep
someValueKindedSomeTypeRep (SomeValueKinded _ (_ :: Value c a)) = R.someTypeRep $ Proxy @a

someValueSomeType :: SomeValue -> SomeType
someValueSomeType (SomeValue ctag (SomeValueKinded _ (_ :: Value c a))) =
  ctagSomeType ctag (Proxy @a)

-- * Instances
--
instance Read SomeValue where
  readPrec = do
    tag :: Some CTag <- readPrec
    dict :: TyDict Ground <- readPrec
    case tag of
      Exists tag' -> readSomeValue tag' dict

instance Parse SomeValue where
  parser = parseSomeValue

instance Show SomeValue where
  show (SomeValue _ (SomeValueKinded _ x)) = show x
