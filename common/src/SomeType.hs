{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module SomeType
  ( SomeType(..)
  , proxySomeType
  , someType
  , tagSomeType
  , someTypeFromConType
  , unitSomeType
  , showSomeType
  )
where

import           Codec.Serialise
import qualified Data.Text                        as Text
import           GHC.Generics                       (Generic)
import qualified Type.Reflection                  as R

import Basis

import Type

--------------------------------------------------------------------------------
-- | 'SomeType' is a serialisable form of 'Type'
data SomeType =
  CSomeType
  { tName :: Name SomeType  -- ^ Extracted from the typerep
  , tCon  :: R.TyCon        -- ^ Con
  , tRep  :: R.SomeTypeRep  -- ^ Kind.Type
  } deriving (Eq, Generic, Ord)

instance NFData SomeType

proxySomeType ::
  forall k a
  . (ReifyTag k, Typeable k, Typeable a)
  => Proxy k -> Proxy a -> SomeType
proxySomeType _ pa =
  CSomeType
    (Name . showSomeTypeRepNoKind $ rep)
    (R.typeRepTyCon $ R.typeRep @k)
    rep
 where rep = R.someTypeRep pa

someType ::
  forall k a
  . (ReifyTag k, Typeable k, Typeable a)
  => Type k a -> SomeType
someType _ = proxySomeType (Proxy @k) (Proxy @a)

someTypeFromConType :: SomeType -> SomeType -> SomeType
someTypeFromConType CSomeType{tCon} CSomeType{tName, tRep} = CSomeType{..}

unitSomeType :: SomeType
unitSomeType = tagSomeType TPoint (Proxy @())

tagSomeType :: forall k a. Typeable a => Tag k -> Proxy a -> SomeType
tagSomeType TPoint a = proxySomeType (Proxy @k) a
tagSomeType TList  a = proxySomeType (Proxy @k) a
tagSomeType TSet   a = proxySomeType (Proxy @k) a
tagSomeType TTree  a = proxySomeType (Proxy @k) a
tagSomeType TDag   a = proxySomeType (Proxy @k) a
tagSomeType TGraph a = proxySomeType (Proxy @k) a

showSomeType :: Bool -> SomeType -> Text
showSomeType showDot CSomeType{tName=(showName -> n), tCon} =
  case R.tyConName tCon of
    "'Point" -> if showDot then "• "<>n else n
    "'List"  -> "["<>n<>"]"
    "'Set"   -> "{"<>n<>"}"
    "'Tree"  -> "♆⇊ "<>n
    "'Dag"   -> "♆⇄ "<>n
    "'Graph" -> "☸ "<>n
    _        -> "??? "<>n

instance Read SomeType where readPrec = failRead
instance Show SomeType where
  show (CSomeType _ tycon sometyperep) =
    show tycon<>":"<>unpack
    -- cut out the middle part of a name: we don't care about the kind
    (if Text.isPrefixOf "Name"  shownRep ||
        Text.isPrefixOf "QName" shownRep
     then Text.takeWhile (/= ' ') shownRep <> " " <>
          (Text.reverse . Text.takeWhile (/= ' ') . Text.reverse $ shownRep)
     else shownRep)
    where shownRep = pack $ show sometyperep
instance Serialise SomeType

instance Serialise a => Serialise (I a) where
  encode (I x) = encode x
  decode = I <$> decode

_showTypeable :: Int -> TypeRep (a :: k) -> ShowS
_showTypeable _ _ =
  undefined
-- showTypeable _ TrType = showChar '*'
-- showTypeable _ rep
--   | isListTyCon tc, [ty] <- tys =
--     showChar '[' . shows ty . showChar ']'
--   | isTupleTyCon tc =
--     showChar '(' . showArgs (showChar ',') tys . showChar ')'
--   where (tc, tys) = splitApps rep
-- showTypeable _ TrTyCon {trTyCon = tycon, trKindVars = []}
--   = showTyCon tycon
-- showTypeable p TrTyCon {trTyCon = tycon, trKindVars = args}
--   = showParen (p > 9) $
--     showTyCon tycon .
--     showChar ' ' .
--     showArgs (showChar ' ') args
-- showTypeable p TrFun {trFunArg = x, trFunRes = r}
--   = showParen (p > 8) $
--     showsPrec 9 x . showString " -> " . showsPrec 8 r
-- showTypeable p TrApp {trAppFun = f, trAppArg = x}
--   = showParen (p > 9) $
--     showsPrec 8 f .
--     showChar ' ' .
--     showsPrec 10 x
