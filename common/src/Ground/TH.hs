module Ground.TH
  (defineGroundTypes)
where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Basis

defineGroundTypes :: Q [Dec] -> Q [Dec]
defineGroundTypes qDec = qDec >>= emit
 where
   emit :: [Dec] -> Q [Dec]
   emit [DataD [] dataNameRaw [ty] Nothing cons []] = pure
     [ DataD
         []                         -- no context
         dataName
         [ty]                       -- retain the tyvar binder
         Nothing                    -- no kind spec
         (mkCon <$> gDecls)
         []                         -- no derive clauses
     , SigD
         (mkName "groundTypes")
         (AppT
           (ConT $ mkName "TyDicts")
           (ConT $ mkName "Ground"))
     , ValD
         (VarP $ mkName "groundTypes")
         (NormalB groundDictExp)
         []
     ]
    where
      dataName = nameToDyn dataNameRaw
      nameToDyn :: Name -> Name
      nameToDyn (Name (OccName n) _) = Name (OccName n) NameS
      groundDictExp :: Exp
      groundDictExp = foldr step (VarE (qualRef "Dict" "empty")) gDecls
        where
          step :: GroundDecl -> Exp -> Exp
          step GroundDecl{..} acc =
            AppE (AppE (AppE (VarE (qualRef "Dict" "insert"))
                             (LitE (StringL gdStem)))
                       (AppTypeE (ConE $ mkName "Proxy")
                                 gdTy))
                 acc
      qualRef :: String -> String -> Name
      qualRef qual name = Name (OccName name) (NameQ $ ModName qual)
      gDecls :: [GroundDecl]
      gDecls = conDecl <$> cons
      conDecl :: Con -> GroundDecl
      conDecl con =
        case con of
          GadtC ns [] retTy ->
            case nameOf con of
              ('V':stem, name) -> GroundDecl stem (nameToDyn name) retTy
              (other, _) -> error $
                "Ground tag constructor name doesn't start with V: " <> other
          GadtC _ _ _  -> error $
            "Ground tag specifier allows no arguments: " <> fst (nameOf con)
      nameOf :: Con -> (String, Name)
      nameOf = \case
        GadtC [] _ _      -> error $ "Broken GADT variant:  no name"
        GadtC (_:_:_) _ _ -> error $ "Broken GADT variant:  multiple names"
        GadtC [name@(Name (OccName str) _)] _ _ -> (str, name)
        _                 -> error $ "Non-GADT constructor"
      mkCon :: GroundDecl -> Con
      mkCon GroundDecl{..} = GadtC [gdName] [] (promoteTyToTag  gdTy)
      promoteTyToTag :: Type -> Type
      promoteTyToTag gdTy =
        AppT (ConT dataName) gdTy

data GroundDecl
  = GroundDecl
    { gdStem :: String
    , gdName :: Name
    , gdTy   :: Type
    }

-- http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Lib.html
-- mkName :: String -> Name
-- type BangType = (Bang, Type)
-- type VarBangType = (Name, Bang, Type)
-- type Cxt = [Pred]	                -- (Eq a, Ord b)
-- data Type
--   AppT Type Type                     -- T a b
--   VarT Name                          -- a
--   ConT Name	                        -- T
--   ParensT Type
--   ArrowT
-- data Bang
--   Bang SourceUnpackedness SourceStrictness -- C { {-# UNPACK #-} !}a
-- data Body
--   NormalB Exp                        -- f p { = e } where ds
-- data Dec
--   ValD Pat Body [Dec]
--   DataD Cxt Name [TyVarBndr ()] (Maybe Kind) [Con] [DerivClause]
-- data Pat
--   VarP Name
--   ConP Name [Pat]                    -- {C1 p1 p1} = e
--   InfixP Pat Name Pat                -- foo ({x :+ y}) = e
--   ParensP Pat
--   RecP Name [FieldPat]               -- f (Pt { pointx = x }) = g x
--   ListP [Pat]
--   SigP Pat Type
-- data Con
--   NormalC Name [BangType]            -- C Int a
--   RecC Name [VarBangType]            -- C { v :: Int, w :: a }
--   GadtC [Name] [BangType] Type       -- C :: a -> b -> T b Int
--   RecGadtC [Name] [VarBangType] Type -- C :: { v :: Int } -> T b Int
-- data Exp
--   VarE Name                          -- { x }
--   ConE Name                          -- data T1 = C1 t1 t2; p = {C1} e1 e2
--   LitE Lit                           --  { 5 or 'c'}
--   AppE Exp Exp                       --  { f x }
