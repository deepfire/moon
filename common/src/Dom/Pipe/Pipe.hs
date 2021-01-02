module Dom.Pipe.Pipe (module Dom.Pipe.Pipe) where

import Algebra.Graph                    qualified as G
import Data.Dynamic                                 (toDyn)

import Dom.CTag
import Dom.Cap
import Dom.Name
import Dom.Pipe
import Dom.Pipe.IOA
import Dom.Pipe.SomePipe
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
  forall ioa c0 t0.
  ( ioa ~ IOA '[] (CTagV c0 t0)
  , ReifyCTag c0, ReifyVTag t0, Typeable c0, Typeable t0)
  => Name Pipe
  -> CTagV c0 t0
  -> Result (Repr c0 t0)
  -> Pipe '[] (CTagV c0 t0) Dynamic
pipe0 n (typesTags -> ts0) mf =
  Pipe (Desc n sig str (dynRep dyn) Nil ts0) dyn
 where
   dyn = toDyn (IOA mf Proxy Proxy :: ioa)
   sig = Sig [] (I $ tagsSomeType ts0)
   str = Struct G.empty
  --       graph   = G.vertex ty

pipe1 ::
  forall ioa c1 t1 c0 t0.
  ( ioa ~ IOA '[CTagV c1 t1] (CTagV c0 t0)
  , ReifyCTag c0, ReifyVTag t0, Typeable c0, Typeable t0
  , ReifyCTag c1, ReifyVTag t1, Typeable c1, Typeable t1)
  => Name Pipe
  -> CTagV c1 t1 -> CTagV c0 t0
  -> (Repr c1 t1 -> Result (Repr c0 t0))
  -> Pipe '[CTagV c1 t1] (CTagV c0 t0) Dynamic
pipe1 n (typesTags -> ts1) (typesTags -> ts0) mf =
  Pipe (Desc n sig str (dynRep dyn) (ts1 :* Nil) ts0) dyn
 where
   dyn = toDyn (IOA mf Proxy Proxy :: ioa)
   sig = Sig [I $ tagsSomeType ts1] (I $ tagsSomeType ts0)
   str = Struct G.empty
   -- G.connect (G.vertex $ sIn sig) (G.vertex $ sOut sig)

pipe2 ::
  forall ioa c2 t2 c1 t1 c0 t0.
  ( ioa ~ IOA '[CTagV c2 t2, CTagV c1 t1] (CTagV c0 t0)
  , ReifyCTag c0, ReifyVTag t0, Typeable c0, Typeable t0
  , ReifyCTag c1, ReifyVTag t1, Typeable c1, Typeable t1
  , ReifyCTag c2, ReifyVTag t2, Typeable c2, Typeable t2)
  => Name Pipe
  -> CTagV c2 t2 -> CTagV c1 t1 -> CTagV c0 t0
  -> (Repr c2 t2 -> Repr  c1 t1 -> Result (Repr c0 t0))
  -> Pipe '[CTagV c2 t2, CTagV c1 t1] (CTagV c0 t0) Dynamic
pipe2 n (typesTags -> ts2) (typesTags -> ts1) (typesTags -> ts0) mf =
  Pipe (Desc n sig str (dynRep dyn) (ts2 :* ts1 :* Nil) ts0) dyn
 where
   dyn = toDyn (IOA mf Proxy Proxy :: ioa)
   sig = Sig [I $ tagsSomeType ts2, I $ tagsSomeType ts1] (I $ tagsSomeType ts0)
   str = Struct G.empty

pipe3 ::
  forall ioa c3 t3 c2 t2 c1 t1 c0 t0.
  ( ioa ~ IOA '[CTagV c3 t3, CTagV c2 t2, CTagV c1 t1] (CTagV c0 t0)
  , ReifyCTag c0, ReifyVTag t0, Typeable c0, Typeable t0
  , ReifyCTag c1, ReifyVTag t1, Typeable c1, Typeable t1
  , ReifyCTag c2, ReifyVTag t2, Typeable c2, Typeable t2
  , ReifyCTag c3, ReifyVTag t3, Typeable c3, Typeable t3)
  => Name Pipe
  -> CTagV c3 t3 -> CTagV c2 t2 -> CTagV c1 t1 -> CTagV c0 t0
  -> (Repr c3 t3 -> Repr  c2 t2 -> Repr  c1 t1 -> Result (Repr c0 t0))
  -> Pipe '[CTagV c3 t3, CTagV c2 t2, CTagV c1 t1] (CTagV c0 t0) Dynamic
pipe3 n (typesTags -> ts3) (typesTags -> ts2) (typesTags -> ts1) (typesTags -> ts0) mf =
  Pipe (Desc n sig str (dynRep dyn) (ts3 :* ts2 :* ts1 :* Nil) ts0) dyn
 where
   dyn = toDyn (IOA mf Proxy Proxy :: ioa)
   sig = Sig [ I $ tagsSomeType ts3
             , I $ tagsSomeType ts2
             , I $ tagsSomeType ts1
             ] (I $ tagsSomeType ts0)
   str = Struct G.empty

somePipe0 ::
  forall c0 t0.
  (ReifyCTag c0, ReifyVTag t0, Typeable c0, Typeable t0)
  => Name Pipe
  -> Caps t0
  -> CTagV c0 t0
  -> Result (Repr c0 t0)
  -> SomePipe Dynamic
somePipe0 n c t0 pf = SP mempty c $ pipe0 n t0 pf

somePipe1 ::
  forall c1 t1 c0 t0.
  ( ReifyCTag c1, ReifyVTag t1, Typeable c1, Typeable t1
  , ReifyCTag c0, ReifyVTag t0, Typeable c0, Typeable t0)
  => Name Pipe
  -> Caps t0
  -> CTagV c1 t1 -> CTagV c0 t0
  -> (Repr c1 t1 -> Result (Repr c0 t0))
  -> SomePipe Dynamic
somePipe1 n c t1 t0 pf = SP mempty c $ pipe1 n t1 t0 pf

somePipe2 ::
  forall c2 t2 c1 t1 c0 t0.
  ( ReifyCTag c2, ReifyVTag t2, Typeable c2, Typeable t2
  , ReifyCTag c1, ReifyVTag t1, Typeable c1, Typeable t1
  , ReifyCTag c0, ReifyVTag t0, Typeable c0, Typeable t0)
  => Name Pipe
  -> Caps t0
  -> CTagV c2 t2 -> CTagV c1 t1 -> CTagV c0 t0
  -> (Repr c2 t2 -> Repr  c1 t1 -> Result (Repr c0 t0))
  -> SomePipe Dynamic
somePipe2 n c t2 t1 t0 pf = SP mempty c $ pipe2 n t2 t1 t0 pf

somePipe3 ::
  forall c3 t3 c2 t2 c1 t1 c0 t0.
  ( ReifyCTag c3, ReifyVTag t3, Typeable c3, Typeable t3
  , ReifyCTag c2, ReifyVTag t2, Typeable c2, Typeable t2
  , ReifyCTag c1, ReifyVTag t1, Typeable c1, Typeable t1
  , ReifyCTag c0, ReifyVTag t0, Typeable c0, Typeable t0)
  => Name Pipe
  -> Caps t0
  -> CTagV c3 t3 -> CTagV c2 t2 -> CTagV c1 t1 -> CTagV c0 t0
  -> (Repr c3 t3 -> Repr  c2 t2 -> Repr  c1 t1 -> Result (Repr c0 t0))
  -> SomePipe Dynamic
somePipe3 n c t3 t2 t1 t0 pf = SP mempty c $ pipe3 n t3 t2 t1 t0 pf
