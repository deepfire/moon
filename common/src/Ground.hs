{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}

module Ground
  ( Dict(..)
  , Dicts(..)
  , Some(..)
  -- * Ground types & RTTI
  , Ground.lookup
  , withGroundType
  , rtti2Rep
  , rtti2Tag
  -- * Re-exports
  , module Type
  , module Ground.Hask
  -- * ...
  , groundTypeList
    -- * Namespace
  , spacePipe
  )
where

import qualified Data.Map                         as Map
import           Text.Read                          (Lexeme(..), lexP)
import           Type.Reflection                    (SomeTypeRep, someTypeRep)
import qualified Unsafe.Coerce                    as Unsafe

import Basis
import Data.Dict
import qualified Data.Dict as Dict
import Data.RTTI
import Data.Some
import Ground.Hask
import qualified Ground.Hask as Hask
import Namespace
import Pipe hiding (link)
import Type


spacePipe :: QName (Scope Point SomePipe) -> Space Point SomePipe
spacePipe graft = mempty
  & attachScopes (graft)
      [ dataProjScopeG $ Proxy @Con
      -- , dataProjScope  $ Proxy @Expr
      ]

-- * Ground API
--
lookup :: SomeTypeRep -> Maybe (Dict Ground)
lookup = Dict.lookup groundTypes

withGroundType :: SomeTypeRep -> (Dict Ground -> b) -> Maybe b
withGroundType str f =
  f <$> Ground.lookup str

rtti2Tag :: RTTI2 Tag k a -> Tag' k
rtti2Tag x = (Unsafe.unsafeCoerce getter) x
  where getter :: RTTI2 Tag k Con -> Tag' k
        getter (TCon tag _) = tag

rtti2Rep :: RTTI2 Tag k a -> SomeTypeRep
rtti2Rep x = (Unsafe.unsafeCoerce getter) x
  where getter :: RTTI2 Tag k Con -> SomeTypeRep
        getter (TCon _ rep) = rep


-- * Tables
--
data instance RTTI2 Tag (k :: Con) (a :: *) where
  TCon                 :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Con
  TType                :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Type
  TSig                 :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Sig
  TStruct              :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Struct
  TSomeTypeRep         :: Tag' k -> SomeTypeRep -> RTTI2 Tag k SomeTypeRep
  TNameType            :: Tag' k -> SomeTypeRep -> RTTI2 Tag k (Name Type)
  TNamePipe            :: Tag' k -> SomeTypeRep -> RTTI2 Tag k (Name Pipe)

  TInt                 :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Int
  TInteger             :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Integer
  TFloat               :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Float
  TDouble              :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Double
  TString              :: Tag' k -> SomeTypeRep -> RTTI2 Tag k String
  TText                :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Text
  TUnit                :: Tag' k -> SomeTypeRep -> RTTI2 Tag k ()

  THaskNameIndex       :: Tag' k -> SomeTypeRep -> RTTI2 Tag k (Name Hask.Index)
  THaskNameRepo        :: Tag' k -> SomeTypeRep -> RTTI2 Tag k (Name Hask.Repo)
  THaskNamePackage     :: Tag' k -> SomeTypeRep -> RTTI2 Tag k (Name Hask.Package)
  THaskNameModule      :: Tag' k -> SomeTypeRep -> RTTI2 Tag k (Name Hask.Module)
  THaskNameDef         :: Tag' k -> SomeTypeRep -> RTTI2 Tag k (Name Hask.Def)

groundTypeList :: [SomeTypeRep]
groundTypeList = Map.keys m
  where (Dicts m) = groundTypes

groundTypes :: Dicts Ground
groundTypes = Dicts $ Map.fromList
  [ -- Meta
    link $ Proxy @Con
  , link $ Proxy @Type
  , link $ Proxy @Sig
  , link $ Proxy @Struct
  , link $ Proxy @SomeTypeRep
  , link $ Proxy @(Name Type)
  , link $ Proxy @(Name Pipe)
    -- Atom
  , link $ Proxy @Int
  , link $ Proxy @Integer
  , link $ Proxy @Float
  , link $ Proxy @Double
  , link $ Proxy @String
  , link $ Proxy @Text
  , link $ Proxy @()
    -- Plain
  , link $ Proxy @(Name Hask.FileName)
  , link $ Proxy @(Name Hask.Loc)
  , link $ Proxy @(Name Hask.URL)
    --
  , link $ Proxy @Hask.Index
  , link $ Proxy @(Name Hask.Index)
  , link $ Proxy @(Name Hask.Repo)
  , link $ Proxy @(Name Hask.Package)
  , link $ Proxy @(Name Hask.Module)
  , link $ Proxy @(Name Hask.Def)
  , link $ Proxy @(Name Hask.DefType)
  ]

instance Read (Some2 (RTTI2 Tag)) where
  readPrec = do
    tag <- readPrec
    case tag of
      Exists (x :: Tag' c) -> do
        ty <- lexP
        case ty of
          Ident "Con"             -> pure $ mkRTTI TCon              x
          Ident "Type"            -> pure $ mkRTTI TType             x
          Ident "Sig"             -> pure $ mkRTTI TSig              x
          Ident "Struct"          -> pure $ mkRTTI TStruct           x
          Ident "SomeTypeRep"     -> pure $ mkRTTI TSomeTypeRep      x
          Ident "NameType"        -> pure $ mkRTTI TNameType         x
          Ident "NamePipe"        -> pure $ mkRTTI TNamePipe         x

          Ident "Int"             -> pure $ mkRTTI TInt              x
          Ident "Integer"         -> pure $ mkRTTI TInteger          x
          Ident "Float"           -> pure $ mkRTTI TFloat            x
          Ident "Double"          -> pure $ mkRTTI TDouble           x
          Ident "String"          -> pure $ mkRTTI TString           x
          Ident "Text"            -> pure $ mkRTTI TText             x
          Ident "Unit"            -> pure $ mkRTTI TUnit             x

          Ident "HaskIndexName"   -> pure $ mkRTTI THaskNameIndex    x
          Ident "HaskRepoName"    -> pure $ mkRTTI THaskNameRepo     x
          Ident "HaskPackageName" -> pure $ mkRTTI THaskNamePackage  x
          Ident "HaskModuleName"  -> pure $ mkRTTI THaskNameModule   x
          Ident "HaskDefName"     -> pure $ mkRTTI THaskNameDef      x
          _ -> trace (printf "Unknown Tag: %s" (show ty))
                     (fail "")
      where
        mkRTTI
          :: forall k a
          . Typeable a
          => (Tag' k -> SomeTypeRep -> RTTI2 Tag k a)
          -> Tag' k
          -> Some2 (RTTI2 Tag)
        mkRTTI con tag = Exists2 $ con tag (someTypeRep $ Proxy @a)
