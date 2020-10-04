{-# LANGUAGE UndecidableSuperClasses    #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module SomeType
  ( SomeCTag(..)
  , SomeType(..)
  , proxySomeType
  , someType
  , ctagSomeType
  , someTypeFromConType
  , unitSomeType
  , showSomeType
  )
where

import qualified Codec.CBOR.Decoding              as CBOR (decodeWord)
import qualified Codec.CBOR.Encoding              as CBOR (encodeWord)
import           Codec.Serialise
import qualified Data.Text                        as Text
import           GHC.Generics                       (Generic)
import qualified Type.Reflection                  as R

import Basis

import Type

--------------------------------------------------------------------------------
data SomeCTag where
  SomeCTag
    :: (ReifyCTag c, Typeable c)
    => CTag (c :: Con) -> SomeCTag

instance Serialise SomeCTag where
  encode = CBOR.encodeWord . \(SomeCTag tag) -> case tag of
    TPoint -> 1
    TList  -> 2
    TSet   -> 3
    TTree  -> 4
    TDag   -> 5
    TGraph -> 6
  decode = do
    tag <- CBOR.decodeWord
    case tag of
      1 -> pure $ SomeCTag TPoint
      2 -> pure $ SomeCTag TList
      3 -> pure $ SomeCTag TSet
      4 -> pure $ SomeCTag TTree
      5 -> pure $ SomeCTag TDag
      6 -> pure $ SomeCTag TGraph
      _ -> fail $ "invalid SomeCTag encoding: tag="<>show tag

--------------------------------------------------------------------------------
-- | 'SomeType' is a serialisable form of 'Type'
data SomeType =
  CSomeType
  { tName :: Name SomeType  -- ^ Extracted from the typerep
  , tCon  :: R.TyCon        -- ^ Con
  , tRep  :: R.SomeTypeRep  -- ^ cind.Type
  } deriving (Eq, Generic, Ord)

instance NFData SomeType

proxySomeType ::
  forall c a
  . (ReifyCTag c, Typeable c, Typeable a)
  => Proxy c -> Proxy a -> SomeType
proxySomeType _ pa =
  CSomeType
    (Name . showSomeTypeRepNoKind $ rep)
    (R.typeRepTyCon $ R.typeRep @c)
    rep
 where rep = R.someTypeRep pa

someType ::
  forall c a
  . (ReifyCTag c, Typeable c, Typeable a)
  => Type c a -> SomeType
someType _ = proxySomeType (Proxy @c) (Proxy @a)

someTypeFromConType :: SomeType -> SomeType -> SomeType
someTypeFromConType CSomeType{tCon} CSomeType{tName, tRep} = CSomeType{..}

unitSomeType :: SomeType
unitSomeType = ctagSomeType TPoint (Proxy @())

ctagSomeType :: forall c a. Typeable a => CTag c -> Proxy a -> SomeType
ctagSomeType TPoint a = proxySomeType (Proxy @c) a
ctagSomeType TList  a = proxySomeType (Proxy @c) a
ctagSomeType TSet   a = proxySomeType (Proxy @c) a
ctagSomeType TTree  a = proxySomeType (Proxy @c) a
ctagSomeType TDag   a = proxySomeType (Proxy @c) a
ctagSomeType TGraph a = proxySomeType (Proxy @c) a

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
    -- cut out the middle part of a name: we don't care about the cind
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

_showTypeable :: Int -> TypeRep (a :: c) -> ShowS
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
