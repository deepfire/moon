data SomeKindValue a =
  forall (c :: Con). Typeable c =>
  SomeKindValue (CTag c) (Value c a)

instance Eq (SomeValueKinded c) where
  SomeValueKinded _ (va :: Value c a) == SomeValueKinded _ (vb :: Value c b) =
    case typeRep @a `eqTypeRep` typeRep @b of
      Just HRefl -> stripValue va == stripValue vb
      Nothing -> False

instance Ord (SomeValueKinded c) where
  SomeValueKinded vta (va :: Value c a) `compare`
   SomeValueKinded vtb (vb :: Value c b) =
    case typeRep @a `eqTypeRep` typeRep @b of
      Just HRefl -> stripValue va `compare` stripValue vb
      Nothing -> vta `compare` vtb

stripSomeValue ::
  forall a c
   . (Typeable a, Typeable c)
  => CTag c
  -> Proxy a
  -> SomeValue
  -> Maybe (Repr c a)
stripSomeValue _ _ (SomeValue (_ :: CTag c') (SomeValueKinded (r :: Value c' a'))) =
  let exptr = typeRep @a
      svtr  = typeRep @a'
      expk  = typeRep @c
      svk   = typeRep @c'
  in case (,) (svtr `eqTypeRep` exptr)
              (svk  `eqTypeRep` expk) of
    (Just HRefl, Just HRefl) -> Just $ stripValue r
    _ -> Nothing
