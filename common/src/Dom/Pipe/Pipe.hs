{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Dom.Pipe.Pipe (module Dom.Pipe.Pipe) where

import Algebra.Graph                    qualified as G
import Data.Dynamic                                 (toDyn)

import Dom.CTag
import Dom.Cap
import Dom.LTag
import Dom.Name
import Dom.Pipe
import Dom.Pipe.IOA
import Dom.Pipe.SomePipe
import Dom.Result
import Dom.Sig
import Dom.SomeType
import Dom.Struct
import Dom.Tags
import Dom.VTag

import Basis


--------------------------------------------------------------------------------
-- * Pipe construction
--
-- [Note: Type argument numbering]
--
-- In order to maintain stable & simple enumeration of arrow type constituents,
-- the enumeration starts from end -- i.e:
--   - the arrow result is #0
--   - the last function argument, if any, is #1
--   and so on.
pipe0 ::
  forall ioa l c0 v0.
  ( ioa ~ IOA Now '[] (CTagV c0 v0)
  , ReifyCTag c0, ReifyVTag v0, Typeable c0, Typeable v0)
  => Name Pipe
  -> LTag l
  -> CTagV c0 v0
  -> Result l (Repr c0 v0)
  -> Pipe l '[] (CTagV c0 v0) Dynamic
pipe0 n l@LNow (typesTags -> ts0) mf =
  Pipe (Desc n sig str (dynRep dyn) l Nil ts0) dyn
 where
   dyn = toDyn (IOA mf Proxy Proxy :: ioa)
   sig = Sig [] (I $ tagsSomeType ts0)
   str = Struct G.empty
  --       graph   = G.vertex ty

pipe1 ::
  forall ioa l c1 v1 c0 v0.
  ( ioa ~ IOA Now '[CTagV c1 v1] (CTagV c0 v0)
  , ReifyCTag c0, ReifyVTag v0, Typeable c0, Typeable v0
  , ReifyCTag c1, ReifyVTag v1, Typeable c1, Typeable v1)
  => Name Pipe
  -> LTag l
  -> CTagV c1 v1 -> CTagV c0 v0
  -> (Repr c1 v1 -> Result l (Repr c0 v0))
  -> Pipe l '[CTagV c1 v1] (CTagV c0 v0) Dynamic
pipe1 n l@LNow (typesTags -> ts1) (typesTags -> ts0) mf =
  Pipe (Desc n sig str (dynRep dyn) l (ts1 :* Nil) ts0) dyn
 where
   dyn = toDyn (IOA mf Proxy Proxy :: ioa)
   sig = Sig [I $ tagsSomeType ts1] (I $ tagsSomeType ts0)
   str = Struct G.empty
   -- G.connect (G.vertex $ sIn sig) (G.vertex $ sOut sig)

pipe2 ::
  forall ioa l c2 v2 c1 v1 c0 v0.
  ( ioa ~ IOA Now '[CTagV c2 v2, CTagV c1 v1] (CTagV c0 v0)
  , ReifyCTag c0, ReifyVTag v0, Typeable c0, Typeable v0
  , ReifyCTag c1, ReifyVTag v1, Typeable c1, Typeable v1
  , ReifyCTag c2, ReifyVTag v2, Typeable c2, Typeable v2)
  => Name Pipe
  -> LTag l
  -> CTagV c2 v2 -> CTagV c1 v1 -> CTagV c0 v0
  -> (Repr c2 v2 -> Repr  c1 v1 -> Result l (Repr c0 v0))
  -> Pipe l '[CTagV c2 v2, CTagV c1 v1] (CTagV c0 v0) Dynamic
pipe2 n l@LNow (typesTags -> ts2) (typesTags -> ts1) (typesTags -> ts0) mf =
  Pipe (Desc n sig str (dynRep dyn) l (ts2 :* ts1 :* Nil) ts0) dyn
 where
   dyn = toDyn (IOA mf Proxy Proxy :: ioa)
   sig = Sig [I $ tagsSomeType ts2, I $ tagsSomeType ts1] (I $ tagsSomeType ts0)
   str = Struct G.empty

pipe3 ::
  forall ioa l c3 v3 c2 v2 c1 v1 c0 v0.
  ( ioa ~ IOA Now '[CTagV c3 v3, CTagV c2 v2, CTagV c1 v1] (CTagV c0 v0)
  , ReifyCTag c0, ReifyVTag v0, Typeable c0, Typeable v0
  , ReifyCTag c1, ReifyVTag v1, Typeable c1, Typeable v1
  , ReifyCTag c2, ReifyVTag v2, Typeable c2, Typeable v2
  , ReifyCTag c3, ReifyVTag v3, Typeable c3, Typeable v3)
  => Name Pipe
  -> LTag l
  -> CTagV c3 v3 -> CTagV c2 v2 -> CTagV c1 v1 -> CTagV c0 v0
  -> (Repr c3 v3 -> Repr  c2 v2 -> Repr  c1 v1 -> Result l (Repr c0 v0))
  -> Pipe l '[CTagV c3 v3, CTagV c2 v2, CTagV c1 v1] (CTagV c0 v0) Dynamic
pipe3 n l@LNow (typesTags -> ts3) (typesTags -> ts2) (typesTags -> ts1) (typesTags -> ts0) mf =
  Pipe (Desc n sig str (dynRep dyn) l (ts3 :* ts2 :* ts1 :* Nil) ts0) dyn
 where
   dyn = toDyn (IOA mf Proxy Proxy :: ioa)
   sig = Sig [ I $ tagsSomeType ts3
             , I $ tagsSomeType ts2
             , I $ tagsSomeType ts1
             ] (I $ tagsSomeType ts0)
   str = Struct G.empty

somePipe0 ::
  forall l c0 v0.
  (ReifyCTag c0, ReifyVTag v0, Typeable c0, Typeable v0
  , LiveConstr l)
  => Name Pipe
  -> LTag l
  -> Caps v0
  -> CTagV c0 v0
  -> Result l (Repr c0 v0)
  -> SomePipe Dynamic
somePipe0 n l c v0 pf = SP mempty c $ pipe0 n l v0 pf

somePipe1 ::
  forall l c1 v1 c0 v0.
  ( ReifyCTag c1, ReifyVTag v1, Typeable c1, Typeable v1
  , ReifyCTag c0, ReifyVTag v0, Typeable c0, Typeable v0
  , LiveConstr l)
  => Name Pipe
  -> LTag l
  -> Caps v0
  -> CTagV c1 v1 -> CTagV c0 v0
  -> (Repr c1 v1 -> Result l (Repr c0 v0))
  -> SomePipe Dynamic
somePipe1 n l c v1 v0 pf = SP mempty c $ pipe1 n l v1 v0 pf

somePipe2 ::
  forall l c2 v2 c1 v1 c0 v0.
  ( ReifyCTag c2, ReifyVTag v2, Typeable c2, Typeable v2
  , ReifyCTag c1, ReifyVTag v1, Typeable c1, Typeable v1
  , ReifyCTag c0, ReifyVTag v0, Typeable c0, Typeable v0
  , LiveConstr l)
  => Name Pipe
  -> LTag l
  -> Caps v0
  -> CTagV c2 v2 -> CTagV c1 v1 -> CTagV c0 v0
  -> (Repr c2 v2 -> Repr  c1 v1 -> Result l (Repr c0 v0))
  -> SomePipe Dynamic
somePipe2 n l c v2 v1 v0 pf = SP mempty c $ pipe2 n l v2 v1 v0 pf

somePipe3 ::
  forall l c3 v3 c2 v2 c1 v1 c0 v0.
  ( ReifyCTag c3, ReifyVTag v3, Typeable c3, Typeable v3
  , ReifyCTag c2, ReifyVTag v2, Typeable c2, Typeable v2
  , ReifyCTag c1, ReifyVTag v1, Typeable c1, Typeable v1
  , ReifyCTag c0, ReifyVTag v0, Typeable c0, Typeable v0
  , LiveConstr l)
  => Name Pipe
  -> LTag l
  -> Caps v0
  -> CTagV c3 v3 -> CTagV c2 v2 -> CTagV c1 v1 -> CTagV c0 v0
  -> (Repr c3 v3 -> Repr  c2 v2 -> Repr  c1 v1 -> Result l (Repr c0 v0))
  -> SomePipe Dynamic
somePipe3 n l c v3 v2 v1 v0 pf = SP mempty c $ pipe3 n l v3 v2 v1 v0 pf
