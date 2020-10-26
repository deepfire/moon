parseSomePipe :: (QName Pipe -> Maybe (SomePipe ())) -> Opt.Parser (SomePipe ())
parseSomePipe lookupPipe =
  runA $ proc () -> do
    p <- asA (strArgument (metavar "PIPEDESC")) -< ()
    returnA -< case do
      ast <- parse p
      compile opsDesc lookupPipe ast of
      Left  e -> error (unpack e)
      Right x -> x

someEmptyPipe :: Name Pipe -> p -> SomePipe p
someEmptyPipe = G .: emptyPipe

emptyDesc :: Name Pipe -> Desc Ground '[] ()
emptyDesc name = Desc
  { pdName   = name
  , pdSig    = Gen unitType unitType
  , pdStruct = Struct G.empty
  , pdRep    = SomeTypeRep (typeRep @(IOA Ground '[Type Point ()] (Type Point ())))
  , pdArgs   = Nil
  , pdOut    = Proxy @()
  }

emptyPipe :: Name Pipe -> p -> Pipe Ground '[Type Point ()] (Type Point ()) p
emptyPipe = Pipe . emptyDesc

withSomePipe' ::
     forall (p :: *)
  .  SomePipe p
  -> (forall (c :: * -> Constraint)
             (kas  :: [*]) (o  :: *)
             (kas' :: [*]) (o' :: *)
      . (PipeConstr c kas o)
      =>  Pipe c kas  o  p
      -> (Pipe c kas' o' p -> SomePipe p)
      -> SomePipe p)
  -> SomePipe p
withSomePipe' (G x) f = f x G
withSomePipe' (T x) f = f x T
withSomePipe'
  :: forall (p :: *)
  .  SomePipe p
  -> (forall (c :: * -> Constraint)
             (kas  :: [*]) (o  :: *)
             (kas' :: [*]) (o' :: *)
      . (PipeConstr c kas o, PipeConstr c kas' o')
      =>  Pipe c kas  o  p
      -> (Pipe c kas' o' p -> SomePipe p)
      -> SomePipe p)
  -> SomePipe p
withSomePipe' (G x) f = f x G
withSomePipe' (T x) f = f x T

 A useless function : -/
mapSomePipeEither
  :: forall (e :: *) (p :: *) -- (kas' :: [*])
  .  --(All Typeable kas', All PairTypeable kas')
     ()
  => SomePipe p
  -> (forall (c :: * -> Constraint) (kas :: [*]) (kas' :: [*]) (o :: *)
      . (PipeConstr c kas o, PipeConstr c kas' o)
      => (Proxy c -> NP (SOP.K ()) kas -> Proxy o -> NP (SOP.K ()) kas')
      -> Pipe c kas o p
      -> Either e (Pipe c kas' o p))
  -> Either e (SomePipe p)
mapSomePipeEither (G x) f = G <$> f tf x
  where
    tf :: Proxy c -> NP (SOP.K ()) kas -> Proxy o -> NP (SOP.K ()) kas'
    tf c (_ SOP.:* xs) o = xs
mapSomePipeEither (T x) f = T <$> f Proxy x
