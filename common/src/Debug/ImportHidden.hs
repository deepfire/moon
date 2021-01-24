-- As per:
--
--  https://www.tweag.io/blog/2021-01-07-haskell-dark-arts-part-i/
--
-- By Cheng Shao and Richard Eisenberg.
--
module Debug.ImportHidden (importHidden) where

import DynFlags
import FastString
import GHC
import Language.Haskell.TH.Syntax
import Module
import Packages
import TcRnTypes
import Unsafe.Coerce


unsafeRunTcM :: TcM a -> Q a
unsafeRunTcM m = unsafeCoerce (\_ -> m)

qGetDynFlags :: Q DynFlags
qGetDynFlags = unsafeRunTcM getDynFlags

qLookupUnitId :: String -> Q UnitId
qLookupUnitId pkg_name = do
  dflags <- qGetDynFlags
  comp_id <- case lookupPackageName dflags $ PackageName $ fsLit pkg_name of
    Just comp_id -> pure comp_id
    _ -> fail $ "Package not found: " ++ pkg_name
  pure $ DefiniteUnitId $ DefUnitId $ componentIdToInstalledUnitId comp_id

qLookupPkgName :: String -> Q PkgName
qLookupPkgName pkg_name = do
  unit_id <- qLookupUnitId pkg_name
  pure $ PkgName $ unitIdString unit_id

-- | Usage:
--
--   myFunc = $(importHidden "pkg" "Hidden" "func")
--
importHidden :: String -> String -> String -> Q Exp
importHidden pkg_name mod_name val_name = do
  pkg_name' <- qLookupPkgName pkg_name
  pure $
    VarE $
      Name
        (OccName val_name)
        (NameG VarName pkg_name' (ModName mod_name))
