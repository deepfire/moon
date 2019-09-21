{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}

module Moon.Face.Ground
  ( Dict(..)
  , Dicts(..)
  , GroundContext
  , RTTI(..)
  , Some(..)
  , Moon.Face.Ground.lookup
  , withGroundType
  , groundTypes
  , mkRTTI
  , rtti2Rep
  , rtti2Tag
  )
where

import Debug.Trace
import Text.Printf

import           Prelude hiding (lookup)

import           Codec.Serialise                    (Serialise)
import qualified Data.Map                         as Map
import           Data.Proxy
import           Data.Text                          (Text)
import           Data.Typeable                      (Typeable)
import           Text.Read
import           Type.Reflection                    (SomeTypeRep, someTypeRep)
import qualified Unsafe.Coerce                    as Unsafe

import           Data.Dict                          (Dicts(..), link)
import qualified Data.Dict                        as Dict
import           Moon.Face
import qualified Moon.Face.Haskell                as Haskell

lookup :: SomeTypeRep -> Maybe (Dict GroundContext)
lookup = Dict.lookup groundTypes

withGroundType :: SomeTypeRep -> (Dict GroundContext -> b) -> Maybe b
withGroundType str f =
  f <$> Moon.Face.Ground.lookup str

data instance RTTI2 Tag (k :: Con) (a :: *) where
  TCon                 :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Con
  TTypeName            :: Tag' k -> SomeTypeRep -> RTTI2 Tag k TypeName
  TType                :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Type
  TVariantName         :: Tag' k -> SomeTypeRep -> RTTI2 Tag k VariantName
  TVariant             :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Variant
  TFieldName           :: Tag' k -> SomeTypeRep -> RTTI2 Tag k FieldName
  TField               :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Field
  TPipeName            :: Tag' k -> SomeTypeRep -> RTTI2 Tag k PipeName
  TSig                 :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Sig
  TStruct              :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Struct
  TDef                 :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Def
  TScope               :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Scope
  TSomeTypeRep         :: Tag' k -> SomeTypeRep -> RTTI2 Tag k SomeTypeRep
                          
  TInt                 :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Int
  TInteger             :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Integer
  TFloat               :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Float
  TDouble              :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Double
  TString              :: Tag' k -> SomeTypeRep -> RTTI2 Tag k String
  TText                :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Text
  TUnit                :: Tag' k -> SomeTypeRep -> RTTI2 Tag k ()
                          
  THaskellIndexName    :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Haskell.IndexName
  THaskellRepoName     :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Haskell.RepoName
  THaskellPackageName  :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Haskell.PackageName
  THaskellModuleName   :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Haskell.ModuleName
  THaskellDefName      :: Tag' k -> SomeTypeRep -> RTTI2 Tag k Haskell.DefName

rtti2Tag :: RTTI2 Tag k a -> Tag' k
rtti2Tag x = (Unsafe.unsafeCoerce getter) x
  where getter :: RTTI2 Tag k Con -> Tag' k
        getter (TCon tag _) = tag

rtti2Rep :: RTTI2 Tag k a -> SomeTypeRep
rtti2Rep x = (Unsafe.unsafeCoerce getter) x
  where getter :: RTTI2 Tag k Con -> SomeTypeRep
        getter (TCon _ rep) = rep

groundTypes :: Dicts GroundContext
groundTypes = Dicts $ Map.fromList
  [ -- Meta
    link $ Proxy @Con
  , link $ Proxy @TypeName
  , link $ Proxy @Type
  , link $ Proxy @VariantName
  , link $ Proxy @Variant
  , link $ Proxy @FieldName
  , link $ Proxy @Field
  , link $ Proxy @PipeName
  , link $ Proxy @Sig
  , link $ Proxy @Struct
  , link $ Proxy @Def
  , link $ Proxy @Scope
  , link $ Proxy @SomeTypeRep
    -- Atom
  , link $ Proxy @Int
  , link $ Proxy @Integer
  , link $ Proxy @Float
  , link $ Proxy @Double
  , link $ Proxy @String
  , link $ Proxy @Text
  , link $ Proxy @()
    -- Plain
  , link $ Proxy @Haskell.IndexName
  , link $ Proxy @Haskell.RepoName
  , link $ Proxy @Haskell.PackageName
  , link $ Proxy @Haskell.ModuleName
  , link $ Proxy @Haskell.DefName
  ]

instance Read SomeTypeRep where readPrec = failRead

mkRTTI
  :: forall k a
  . Typeable a
  => (Tag' k -> SomeTypeRep -> RTTI2 Tag k a)
  -> Tag' k
  -> Some2 (RTTI2 Tag)
mkRTTI con tag = Exists2 $ con tag (someTypeRep $ Proxy @a)

instance Read (Some2 (RTTI2 Tag)) where
  readPrec = do
    tag <- readPrec
    case tag of
      Exists (x :: Tag' c) -> do
        ty <- lexP
        case ty of
          Ident "Con"                -> pure $ mkRTTI TCon                 x
          Ident "TypeName"           -> pure $ mkRTTI TTypeName            x
          Ident "Type"               -> pure $ mkRTTI TType                x
          Ident "VariantName"        -> pure $ mkRTTI TVariantName         x
          Ident "Variant"            -> pure $ mkRTTI TVariant             x
          Ident "FieldName"          -> pure $ mkRTTI TFieldName           x
          Ident "Field"              -> pure $ mkRTTI TField               x
          Ident "PipeName"           -> pure $ mkRTTI TPipeName            x
          Ident "Sig"                -> pure $ mkRTTI TSig                 x
          Ident "Struct"             -> pure $ mkRTTI TStruct              x
          Ident "Def"                -> pure $ mkRTTI TDef                 x
          Ident "Scope"              -> pure $ mkRTTI TScope               x
          Ident "SomeTypeRep"        -> pure $ mkRTTI TSomeTypeRep         x
                                   
          Ident "Int"                -> pure $ mkRTTI TInt                 x
          Ident "Integer"            -> pure $ mkRTTI TInteger             x
          Ident "Float"              -> pure $ mkRTTI TFloat               x
          Ident "Double"             -> pure $ mkRTTI TDouble              x
          Ident "String"             -> pure $ mkRTTI TString              x
          Ident "Text"               -> pure $ mkRTTI TText                x
          Ident "Unit"               -> pure $ mkRTTI TUnit                x

          Ident "HaskellIndexName"   -> pure $ mkRTTI THaskellIndexName    x
          Ident "HaskellRepoName"    -> pure $ mkRTTI THaskellRepoName     x
          Ident "HaskellPackageName" -> pure $ mkRTTI THaskellPackageName  x
          Ident "HaskellModuleName"  -> pure $ mkRTTI THaskellModuleName   x
          Ident "HaskellDefName"     -> pure $ mkRTTI THaskellDefName      x
          _ -> trace (printf "Unknown Tag: %s" (show ty))
                     (fail "")

