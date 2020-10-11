module Ground.TH
  (defineGroundTypes)
where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Basis

-- | Given a GADT data declaration, define:
--
--   - the tag GADT, as per the declaration
--   - 'groundTypes' table
--   - 'mkSomeValue'
--
defineGroundTypes :: Q [Dec] -> Q [Dec]
defineGroundTypes qDec = qDec >>= pure . emit
 where
   emit :: [Dec] -> [Dec]
   emit [DataD [] dataNameRaw [tyIx@(PlainTV tyIxName)] Nothing cons []] =
     [ DataD
         []                             -- no context
         dataName
         [tyIx]                         -- retain the tyvar binder
         Nothing                        -- no kind spec
         (mkCon <$> decls)
         []                             -- no derive clauses
     , SigD
         (mkName "groundTypes")
         (AppT
           (ConT $ mkName "TyDicts")
           (ConT $ mkName "Ground"))
     , ValD
         (VarP $ mkName "groundTypes")
         (NormalB groundDictExp)
         []
     -- mkSomeValue ::
     --   (Typeable c, ReifyCTag c)
     --   => CTag c -> VTag v -> Repr c v -> SomeValue
     , let { c = mkName "c"; v = mkName "v"; } in
       SigD
         (mkName "mkSomeValue")
         (ForallT
            [PlainTV c, PlainTV v]
            [AppT tcTypeable (VarT c), AppT tcReifyCTag (VarT c)]
            $ funT [ AppT tcCTag (VarT c)
                   , AppT tcVTag (VarT v)
                   , AppT (AppT tcRepr (VarT c)) (VarT v)
                   , tcSomeValue ])
     , FunD
         (mkName "mkSomeValue")
         (mkConMkSomeValue <$> decls)
     ] ++ (mkReifyVTagInstance <$> decls)
    where
     -- mkSomeValue c v        = SomeValue c . SVKG v . mkValue v c
      mkConMkSomeValue :: TagDecl -> Clause
      mkConMkSomeValue TagDecl{..} =
        let { c = mkName "c"; v = mkName "v"; } in
          Clause
            [VarP c, AsP v (RecP (mkName $ 'V':tdStem) [])]
            (NormalB
             $ if isJust tdTy
               then dot [ AppE cSomeValue (VarE c)
                        , AppE cSomeValueKinded (VarE v)
                        , AppE (AppE vmkValue (VarE v)) (VarE c)
                        ]
               else
                AppE vError
                     (LitE (StringL "Cannot create SomeKindedValue from a VTop.")))
            []

      tcTypeable  = ConT $ mkName "Typeable"
      tcReifyCTag = ConT $ mkName "ReifyCTag"
      tcGround    = ConT $ mkName "Ground"
      tcCTag      = ConT $ mkName "CTag"
      tcVTag      = ConT $ mkName "VTag"
      tcRepr      = ConT $ mkName "Repr"
      tcSomeValue = ConT $ mkName "SomeValue"
      cSomeValue  = ConE $ mkName "SomeValue"
      cSomeValueKinded = ConE $ mkName "SomeValueKinded"
      cVTop       = ConE $ mkName "VTop"
      vmkValue    = VarE $ mkName "mkValue"
      vDot        = VarE $ mkName "."
      vError      = VarE $ mkName "error"

      funT :: [Type] -> Type
      funT (fromMaybe (error "Empty list passed to funT -- no fun.")
            . unsnoc -> (xs, x)) =
        foldr (AppT . AppT ArrowT) x xs

      dot :: [Exp] -> Exp
      dot (fromMaybe (error "Empty list passed to dot -- no fun.")
           . unsnoc -> (xs, x)) =
        foldr (flip UInfixE vDot) x xs

      dataName = nameToDyn dataNameRaw

      nameToDyn :: Name -> Name
      nameToDyn (Name (OccName n) _) = Name (OccName n) NameS

      decls :: [TagDecl]
      decls =
        -- TagDecl "Top" Nothing -- the catch-all for non-Ground types.
        -- : -- we interpret the retTy of 'a' as a catch-all.
        (conDecl <$> cons)

      conDecl :: Con -> TagDecl
      conDecl con =
        case con of
          GadtC ns [] retTy ->
            case nameOf con of
              ('V':stem, name) ->
                case retTy of
                  VarT _ -> TagDecl stem Nothing
                  _      -> TagDecl stem (Just retTy)
              (other, _) -> error $
                "Ground tag constructor name doesn't start with V: " <> other
          GadtC _ _ _  -> error $
            "Ground tag specifier allows no arguments: " <> fst (nameOf con)

      mkCon :: TagDecl -> Con
      mkCon TagDecl{..} =
        GadtC [Name (OccName ('V':tdStem)) NameS] []
              (promoteTyToTag $
                 -- Here again, 'Nothing' for the type means catchall.
                 fromMaybe (VarT tyIxName) tdTy)

      groundDictExp :: Exp
      groundDictExp = foldr step (VarE (qualRef "Dict" "empty")) decls
        where
          step :: TagDecl -> Exp -> Exp
          step TagDecl{tdTy = Just ty, ..} acc =
            AppE (AppE (AppE (VarE (qualRef "Dict" "insert"))
                             (LitE (StringL tdStem)))
                       (AppTypeE (ConE $ mkName "Proxy")
                                 ty))
                 acc
          step _ acc = acc    -- skip the catch-all: no ground dict entry made
          qualRef :: String -> String -> Name
          qualRef qual name = Name (OccName name) (NameQ $ ModName qual)

      mkReifyVTagInstance :: TagDecl -> Dec
      mkReifyVTagInstance TagDecl{..} =
        InstanceD
          (maybe (Just Overlappable) (const Nothing) tdTy) -- overlap, if a catchall
          []                            -- no context
          (AppT (ConT $ mkName "ReifyVTag")
                (fromMaybe (VarT $ mkName "a") tdTy))
          [ FunD
            -- reifyVTag = const VSomething
              (mkName "reifyVTag")
              [ Clause
                  []
                  (NormalB $
                     AppE (VarE $ mkName "const")
                          (ConE . mkName $ 'V':tdStem))
                  []
              ]]

      nameOf :: Con -> (String, Name)
      nameOf = \case
        GadtC [] _ _      -> error $ "Broken GADT variant:  no name"
        GadtC (_:_:_) _ _ -> error $ "Broken GADT variant:  multiple names"
        GadtC [name@(Name (OccName str) _)] _ _ -> (str, name)
        _                 -> error $ "Non-GADT constructor"

      promoteTyToTag :: Type -> Type
      promoteTyToTag tdTy =
        AppT (ConT dataName) tdTy

   emit []      = shapeError
   emit (_:_:_) = shapeError

   shapeError =
     error "A single GADT 'data' declaration with 0-arity clauses is required."

data TagDecl
  = TagDecl
    { tdStem :: String
    , tdTy   :: Maybe Type
    }

-- http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Lib.html
-- mkName :: String -> Name
-- type BangType = (Bang, Type)
-- type VarBangType = (Name, Bang, Type)
-- type Cxt = [Pred]	                -- (Eq a, Ord b)
-- TyVarBndr
-- data Type
--   AppT Type Type                     -- T a b
--   VarT Name                          -- a
--   ConT Name	                        -- T
--   ParensT Type
--   ArrowT
--   ForallT [TyVarBndr Specificity] Cxt Type -- forall <vars>. <ctxt> => <type>
-- data Bang
--   Bang SourceUnpackedness SourceStrictness -- C { {-# UNPACK #-} !}a
-- data Body
--   NormalB Exp                        -- f p { = e } where ds
-- data Dec
--   ValD Pat Body [Dec]
--   DataD Cxt Name [TyVarBndr ()] (Maybe Kind) [Con] [DerivClause]
--   FunD Name [Clause]                 -- { f p1 p2 = b where decs }
-- data Clause
--   Clause [Pat] Body [Dec]            -- f { p1 p2 = body where decs }
-- data Pat
--   VarP Name
--   ConP Name [Pat]                    -- {C1 p1 p1} = e
--   InfixP Pat Name Pat                -- foo ({x :+ y}) = e
--   ParensP Pat
--   RecP Name [FieldPat]               -- f (Pt { pointx = x }) = g x
--   ListP [Pat]
--   SigP Pat Type
--   AsP Name Pat                       -- { x @ p }
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
-- data TyVarBndr flag
--   PlainTV Name flag                  -- a
--   KindedTV Name flag Kind            -- (a :: k)
-- data Specificity
--   SpecifiedSpec                      -- a
--   InferredSpec                       -- {a}
