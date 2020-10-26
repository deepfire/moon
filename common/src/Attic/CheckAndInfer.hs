checkAndInfer ::
  forall m e p
  .  (Monad m, e ~ Text)
  => Expr (Either (QName Pipe) (SomePipe p))
  -> Either e (Expr (Either (SomePipe ()) (SomePipe p)))
checkAndInfer expr = go True $ fromExpr expr
 where
   -- toSig :: Either (SomePipe ()) (SomePipe p) -> SomePipe ()
   -- toSig (Left x)  = x
   -- toSig (Right p) = somePipeDesc p
   go :: Bool
      -> ZExpr (Either (QName Pipe) (SomePipe p))
      -> Either e (Expr (Either (SomePipe ()) (SomePipe p)))
   go _ (_, PVal x)          = Right (PVal x)
   go _known z@(parents, PPipe p) = case p of
     Right x   -> Right . PPipe $ Right x
     Left name -> case parents of
       [] -> Left $ "Lacking context to infer unknown: " <> showQName name
       Right PApp{}:_  -> PPipe . Left <$> inferFnApp name z
       Left  PApp{}:_  -> undefined
       Right PComp{}:_ -> undefined
       Left  PComp{}:_ -> undefined
       _ -> Left $ "Child of an atom: " <> showQName name
   inferFnApp
     :: QName Pipe
     -> ZExpr (Either (QName Pipe) (SomePipe p))
     -> Either e (SomePipe ())
   inferFnApp _n = goIFA [] where
     goIFA :: [SomePipe ()]
           -> ZExpr (Either (QName Pipe) (SomePipe p))
           -> Either e (SomePipe ())
     goIFA xs z@(Right (PApp _f x):_ps, _self) = join $
       goIFA <$> ((:)
                  <$> undefined (go False (fromExpr x))
                  <*> pure xs)
             <*> pure (upFromLeft' z)
     goIFA xs _ = undefined xs
