{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}
module Dom.Scope.ADTPipe (module Dom.Scope.ADTPipe) where

import Generics.SOP                     qualified as SOP
import Generics.SOP.Some                qualified as SOP
import Language.Haskell.TH
import Language.Haskell.TH.Syntax            hiding (Name)
import Language.Haskell.TH.Syntax       qualified as TH
import Type.Reflection                  qualified as Refl
import Type.Reflection

import Basis

import Dom.CTag
import Dom.Cap
import Dom.Name
import Dom.Pipe.Pipe
import Dom.Pipe.SomePipe
import Dom.Scope.SomePipe
import Dom.VTag

-- import Ground.Table

import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- * Scope of pipes, that offer projections for a GroundData-constrained ADT.
--
-- dataProjScope
--   :: forall u.
--   ( Typeable u, SOP.HasDatatypeInfo u, SOP.Generic u
--   , ReifyVTag u
--   -- XXX: why the combinatory explosion?
--   , All2 Typeable                 (Code u)
--   , All2 ReifyVTag                (Code u)
--   , All2 (And Typeable  Typeable) (Code u)
--   , All2 (And ReifyVTag Typeable) (Code u)
--   , All2 (And Typeable (And ReifyVTag Typeable)) (Code u)
--   )
--   => Proxy u -> Scope Point (SomePipe Dynamic)
-- dataProjScope  p = dataProjScope' p $ dataProjPipes (T mempty) (Proxy @Typeable) p

-- dataProjScopeG
--   :: forall u. (GroundData u)
--   => Proxy u -> Scope Point (SomePipe Dynamic)
-- dataProjScopeG p = dataProjScope' p $ dataProjPipes (SP mempty) (Proxy @Ground) p

--------------------------------------------------------------------------------
-- * ADT field accessor pipes:
data Foo where
  Foo ::
    { lol  :: Int
    , woot :: String
    , yay  :: Dynamic
    } -> Foo
  deriving Generic

instance SOP.Generic         Foo
instance SOP.HasDatatypeInfo Foo

dataProjScope
  :: forall u. Typeable u
  => Proxy u -> [SomePipe Dynamic] -> SomePipeScope Dynamic
dataProjScope _p ps = pipeScope name ps
  where name  = Name $ pack $ show (Refl.typeRepTyCon (typeRep @u))

adtFieldGetterPipe
  :: forall u a
  . ( Typeable u, ReifyVTag u
    , Typeable a, ReifyVTag a)
  => Caps a
  -> SOP.FieldInfo a
  -> (u -> a)
  -> SomePipe Dynamic
adtFieldGetterPipe caps (SOP.FieldInfo n) getter =
  SP mempty caps $
    pipe1 (Name $ pack n)
          CVPoint
          CVPoint -- XXX: Kind should be be capable of being a non-Point!
          (pure . Right . getter)

dataProjPipes ::
  forall u
  . ( Typeable u
    , SOP.HasTypeData ReifyVTag u
    , SOP.Generic u
    , ReifyVTag u
    , All2 ReifyVTag (Code u)
    , All2 Typeable  (Code u)
    )
  => Proxy u
  -> Q Exp
dataProjPipes (SOP.datatypeInfo ->
                 (SOP.ADT _ _ (cinfos :: NP SOP.ConstructorInfo xss) _)) =
  fmap (AppE $ VarE $ mkName "Prelude.concat") $
  fmap (AppE
          (AppE
             (VarE $ mkName "liftAllFields")
             (AppTypeE (ConE $ mkName "Proxy") (reifyTypeRep $ typeRep @u)))) $
  fmap reifyNP $ sequence $ SOP.hcollapse $
   SOP.hcliftA (Proxy @(All Typeable))
     (\(SOP.Record _ (finfos :: NP SOP.FieldInfo xs)) ->
        K $ fmap reifyNP $ sequence $ SOP.hcollapse $
        SOP.hcliftA (Proxy @Typeable)
          (\(_ :: SOP.FieldInfo a) ->
              K $ mkFieldPipe (reifyTypeRep $ typeRep @a))
          finfos)
     cinfos
 where
   mkFieldPipe :: TH.Type -> Q Exp
   mkFieldPipe ty = do
     isShow      <- deepIsInstance nShow      ty
     isGround    <- deepIsInstance nGround    ty
     isSerialise <- deepIsInstance nSerialise ty
     -- traceM $ "isSerialise " <> show ty <> ": " <> show isSerialise
     pure $ AppE (ConE nFieldFun)
                 (AppE (VarE nadtFieldGetterPipe)
                       (VarE case (,,) isShow isGround isSerialise of
                               (,,) _    True True  -> ncapsTSG
                               (,,) _    True False ->
                                 -- XXX: this is _very_ weird.
                                 trace ("Ground, no Serialise: " <> show ty)
                                 ncapsT
                               (,,) True _    _     -> ncapsTS
                               (,,) _    _    _     -> ncapsT))

   reifyNP :: [Exp] -> Exp
   reifyNP [] = ConE nNil
   reifyNP (x:xs) = InfixE (Just x) (ConE nCons) (Just $ reifyNP xs)
   [nGround, nSerialise, nShow, ncapsT, ncapsTS, ncapsTSG, nNil, nCons
     , nFieldFun, nadtFieldGetterPipe]
     = mkName <$>
     ["Ground", "Serialise", "Show", "capsT", "capsTS", "capsTSG", "Nil", ":*"
     , "FieldFun", "adtFieldGetterPipe"]

deepIsInstance :: TH.Name -> TH.Type -> Q Bool
deepIsInstance n t = case t of
  TH.ConT{}    -> isInstance n [t]
  TH.AppT _f x -> (&&)
                    <$> isInstance n [t]
                    <*> deepIsInstance n x

reifyTypeRep :: TypeRep a -> TH.Type
reifyTypeRep = go
 where
   go :: TypeRep b -> TH.Type
   go (Con c) =
     case tyConName c of
       "[]"      -> ListT
       "(,)"     -> TupleT 2
       "(,,)"    -> TupleT 3
       "(,,,,)"  -> TupleT 4
       "(,,,,,)" -> TupleT 5
       n      -> ConT $ TH.Name (OccName n) (NameQ $ ModName $ tyConModule c)
   go (App f x) =
     AppT (go f) (go x)
   go (Fun x r) =
     (ArrowT `TH.AppT` go x) `TH.AppT` go r
