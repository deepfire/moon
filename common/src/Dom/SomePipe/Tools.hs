module Dom.SomePipe.Tools (module Dom.SomePipe.Tools) where

import Generics.SOP                     qualified as SOP

import Basis

import Dom.CTag
import Dom.Error

import Dom.LTag
import Dom.Pipe
import Dom.Pipe.Constr
import Dom.SomePipe.SomePipe


--------------------------------------------------------------------------------
-- * Utils expensive to compile
--
somePipeUncons
  :: forall (p :: *) (e :: *)
  . ()
  => SomePipe p
  -> (forall (l :: Liveness) (cas :: [*]) (o :: *)
      . (PipeConstr l cas o, cas ~ '[])
      => Pipe l '[] o p -> e)
  -> (forall l
             (o :: *)
             (ka :: *) (cas' :: [*])
      . (PipeConstr l (ka:cas') o, PipeConstr l cas' o)
      => Pipe l (ka:cas') o p -> Either e (Pipe l cas' o p))
  -> Either e (SomePipe p)
somePipeUncons (SP _ _ p@(Pipe Desc {pdArgs = Nil   } _)) nil _  = Left $ nil p
somePipeUncons (SP n c p@(Pipe Desc {pdArgs = _ :* _} _)) _ cons = SP n c <$> cons p

somePipeOutSomeCTagType :: SomePipe p -> (SomeCTag, SomeTypeRep)
somePipeOutSomeCTagType p =
  withSomePipe p pipeOutSomeCTagType

somePipeTraverse
  :: SomePipe p
  -> SomePipe p
  -> (forall l (fa :: *) (fo :: *) (to :: *)
      . ( PipeConstr l (fa:'[]) fo
        , PipeConstr l '[]      to)
      => Pipe l (fa:'[]) fo p
      -> Pipe l '[] to p
      -> Fallible (Pipe l '[] (CTagV (CTagVC to) (CTagVV fo)) p))
  -> Fallible (SomePipe p)
somePipeTraverse (SP _ sf f@(Pipe Desc{pdArgs=_:* Nil, pdLTag = LNow} _)) (SP _ _ t@(Pipe Desc{pdArgs=Nil, pdLTag = LNow} _)) trav = SP mempty sf <$> trav f t
somePipeTraverse f t _ = Left $
  if | fA == 0 -> "Supposed function is saturated."
     | fA  > 1 -> "Non-singular function, arity: "   <> showT fA & Error
     | tA  > 0 -> "Unsaturated traversable, arity: " <> showT tA & Error
     | True    -> "Unknown error."
 where fA = somePipeArity f
       tA = somePipeArity t

somePipeArity :: SomePipe p -> Int
somePipeArity sp = withSomePipe sp $
  \p -> length (SOP.hcollapse . SOP.hmap (K . const ()) . pdArgs $ pDesc p)

somePipeArityCase
  :: forall (p :: *) (r :: *)
  .  SomePipe p
  -- Zero.
  -> (forall l o (as :: [*])
      . (PipeConstr l as o, as ~ '[])
      => Pipe l as o p -> r)
  -- One.
  -> (forall l o (as :: [*]) (a :: *)
      . (PipeConstr l as o, as ~ (a:'[]),  PipeConstr l '[] o)
      => Pipe l as o p -> r)
  -- Infinity.
  -> (forall l o (as :: [*]) (a :: *) (as' :: [*])
      . (PipeConstr l as o, as ~ (a:as'), PipeConstr l as' o)
      => Pipe l as o p -> r)
  -> r
somePipeArityCase (SP _ _ p@(Pipe Desc {pdArgs =      Nil} _)) z _ _ = z p
somePipeArityCase (SP _ _ p@(Pipe Desc {pdArgs = _ :* Nil} _)) _ s _ = s p
somePipeArityCase (SP _ _ p@(Pipe Desc {pdArgs = _ :* _}   _)) _ _ n = n p
