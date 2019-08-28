{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE RecordWildCards            #-}
{-# OPTIONS_GHC -Wextra -Wno-unused-binds -Wno-missing-fields -Wno-all-missed-specialisations -Wno-unused-imports #-}

module Moon.Lift.Haskell
  ( GhcLibDir(..)
  , fileToModule)
where

---------------- Pure
import qualified Algebra.Graph as Graph
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Algebra.Graph (Graph)
import           Control.Arrow
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Maybe
import           Data.Text
import           Data.String
---------------- Effectful
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Exception
import           System.Environment
import           System.FilePath
import           System.IO.Extra
---------------- GHC
import           ApiAnnotation hiding (UnicodeSyntax)
import           Config
import qualified DriverPhases   as Drv
import qualified DriverPipeline as Drv
import           DriverPipeline (PipeState(..), PipeEnv(..))
import           DynFlags
import           EnumSet
import           ErrUtils
import           FastString
import           Fingerprint
import           FileCleanup
import           GHC hiding (Module, Name)
import           GHC.LanguageExtensions
import           HeaderInfo
import           HsSyn
import           HscTypes
import           Lexer
import           Outputable
import           Packages
import qualified Parser
import           Panic
import           Panic
import           PipelineMonad
import           Platform
import           Prelude
import           Pretty
import           RdrName
import           SrcLoc
import           StringBuffer
import           SysTools
---------------- Local
import Moon.Face.Haskell

newtype GhcLibDir = GhcLibDir FilePath deriving Show

fileToHsModule
  :: GhcLibDir
  -> FileName
  -> IO (DynFlags, ParseResult (Located (HsModule GhcPs)))
liftHsModule
  :: DynFlags
  -> Located (HsModule GhcPs)
  -> Module
fileToModule
  :: GhcLibDir
  -> FileName
  -> IO (Either Text Module)

fileToModule libDir hsFile = do
  let parseErr (PFailed _ _ x) = pack $ showSDocUnsafe x
      parseErr _               = ""
  (,) dflags parseR <-
    catchIO (second (id &&& parseErr) <$> fileToHsModule libDir hsFile) $
    pure . (undefined,) . (PFailed undefined undefined undefined,)
         . pack . show
  case parseR of
    (PFailed _ _ _, err) -> do
      pure . Left $ err
    (POk _s m, _) -> do
      -- liftIO $ printSDocLn PageMode dflags stdout (mkCodeStyle CStyle) $ ppr m
      pure . Right $ liftHsModule dflags m

liftHsModule dflags (L _ HsModule{..}) = Module{..}
  where
    modName = ModuleName . pack . fromMaybe "Main" $ moduleNameString . unLoc <$> hsmodName
    modDefs = Map.fromList
              [ (n, hd)
              | hd@(Def _ n _) <- catMaybes $ processHsModDeclLoc <$> hsmodDecls]
    processHsModDeclLoc :: LHsDecl GhcPs -> Maybe Def
    processHsModDeclLoc (L l x) = processHsModDecl (toLoc l) x
    toLoc (RealSrcSpan r) = Loc
                                (FileName $ pack $ unpackFS $ SrcLoc.srcSpanFile r)
                                (srcSpanStartLine r)
                                (srcSpanEndLine r)
                                (srcSpanStartCol r)
                                (srcSpanEndCol r)
    toLoc x = error $ "Unexpected unhelpful src span: " Prelude.<> show x
    name :: RdrName -> DefName
    name = DefName . pack . showSDocUnsafe . ppr
    lnam :: LRdrName -> DefName
    lnam = name . unLoc
    processHsModDecl :: Loc -> HsDecl GhcPs -> Maybe Def
    processHsModDecl l = \case
      TyClD   _ x -> processTyClDecl    x l -- ^ Type or Class Declaration
      InstD   _ x -> processInstDecl    x l -- ^ Instance declaration
      ValD    _ x -> processHsBind      x l -- ^ Value declaration
      ForD    _ x -> processForeignDecl x l -- ^ Foreign declaration
      _ -> Nothing
    processTyClDecl (FamDecl{tcdFam=FamilyDecl{fdLName}}) = Just . Def TypeFam (lnam fdLName)
    processTyClDecl (SynDecl{tcdLName})   = Just . Def TypeSyn (lnam tcdLName)
    processTyClDecl (DataDecl{tcdLName})  = Just . Def Data    (lnam tcdLName)
    processTyClDecl (ClassDecl{tcdLName}) = Just . Def Class   (lnam tcdLName)
    processTyClDecl _                     = const Nothing -- XXX: TTG tail dropped
    processInstDecl :: InstDecl GhcPs -> Loc -> Maybe Def
    processInstDecl (ClsInstD _ x) = case x of
      ClsInstDecl {cid_poly_ty=HsIB{hsib_body=(L _ hsType)}}
        -> \loc-> flip (Def Data) loc <$> getInstanceTypeSummary hsType
      _ -> const Nothing
      -- XXX: handle more instance varieties
    processInstDecl _ = const Nothing
    getInstanceTypeSummary :: HsType GhcPs -> Maybe DefName
    getInstanceTypeSummary HsForAllTy{hst_body}   = getInstanceTypeSummary $ unLoc hst_body
    getInstanceTypeSummary HsQualTy{hst_body}     = getInstanceTypeSummary $ unLoc hst_body
    getInstanceTypeSummary (HsTyVar _ _ idP)      = Just $ lnam idP
    getInstanceTypeSummary (HsAppTy _ f _)        = Just . DefName $ pp $ ppr f
    getInstanceTypeSummary (HsParTy _ x)          = getInstanceTypeSummary $ unLoc x
    getInstanceTypeSummary _                      = Nothing -- XXX: _lots_ dropped
    processHsBind          FunBind{fun_id}        = Just . Def Fun (lnam fun_id)
    processHsBind          VarBind{var_id}        = Just . Def Var (name var_id)
    processHsBind          _                      = const Nothing -- abstr. & patsyns dropped
    processForeignDecl     ForeignImport{fd_name} = Just . Def Foreign (lnam fd_name)
    processForeignDecl     _                      = const Nothing
    pp :: SDoc -> Text
    pp x = pack
      . Pretty.renderStyle Pretty.style
      . runSDoc x $ initSDocContext dflags sty
      where sty = mkCodeStyle CStyle

fileToHsModule (GhcLibDir mLibdir) (FileName srcF) = withTempFile $ \cppedF ->
  runGhc (Just mLibdir) $ do
    dflags0 <- getDynFlags
    let dflags1 = dflags0 {outputFile = Just cppedF
                          -- , verbosity = 6
                          }
    _inst_mods <- setSessionDynFlags dflags1
    hsc_env <- GHC.getSession
    let stop_phase = Drv.HsPp Drv.HsSrcFile
    cppedF <- liftIO $ Drv.compileFile hsc_env stop_phase (unpack srcF, Nothing)
    cpped  <- liftIO $ readFile cppedF
    let srcL       = mkRealSrcLoc (mkFastString $ unpack srcF) 1 1
    src_opts <- liftIO $ getOptionsFromFile dflags0 cppedF
    (dflags2, _unhandled_flags, _warns)
      <- liftIO $ parseDynamicFilePragma dflags1 src_opts
    let dflags3 = dflags2 { extensionFlags = Prelude.foldr EnumSet.insert
                                             (extensionFlags dflags2)
                                             alwaysEnabledLanguageExtensions }
    r      <- pure
              . (dflags3,)
              . Lexer.unP Parser.parseModule
              . mkPState dflags3 (stringToStringBuffer cpped) $ srcL
    pure r

alwaysEnabledLanguageExtensions :: [Extension]
alwaysEnabledLanguageExtensions =
  [ LambdaCase
  , Arrows
  , ForeignFunctionInterface
  , GHCForeignImportPrim
  , JavaScriptFFI
  , TemplateHaskell
  , TemplateHaskellQuotes
  , QuasiQuotes
  , UnboxedTuples
  , UnboxedSums
  , BangPatterns
  , RankNTypes
  , ExplicitForAll
  , TypeFamilies
  , TypeFamilyDependencies
  , NumDecimals
  , RecordWildCards
  , RecordPuns
  , ViewPatterns
  , GADTs
  , NPlusKPatterns
  , DoAndIfThenElse
  , BlockArguments
  , RebindableSyntax
  , DataKinds
  , InstanceSigs
  , StandaloneDeriving
  , MagicHash
  , UnicodeSyntax
  , FunctionalDependencies
  , NullaryTypeClasses
  , EmptyDataDecls
  , RecursiveDo
  , PostfixOperators
  , TupleSections
  , PatternGuards
  , TypeOperators
  , PackageImports
  , MultiWayIf
  , BinaryLiterals
  , NegativeLiterals
  , HexFloatLiterals
  , PartialTypeSignatures
  , NamedWildCards
  , StaticPointers
  , TypeApplications
  ]
