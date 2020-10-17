module Dom.SomeType (module Dom.SomeType) where

import           Codec.Serialise                    (Serialise(..))
import           Control.DeepSeq                    (NFData(..))
import           Data.Text                          (Text)
import qualified Data.Text                        as Text
import           Data.Typeable                      (Proxy(..), Typeable)
import           GHC.Generics                       (Generic)
import qualified Text.Builder                     as TB
import           Type.Reflection
import qualified Type.Reflection                  as R

import Basis

import Dom.CTag
import Dom.Name

--------------------------------------------------------------------------------
-- * SomeType: a serialisable form of 'Type'
--
data SomeType =
  CSomeType
  { tName :: Name SomeType  -- ^ Extracted from the typerep
  , tCon  :: TyCon          -- ^ Con
  , tRep  :: SomeTypeRep    -- ^ cind.Type
  } deriving (Eq, Generic, Ord)

--------------------------------------------------------------------------------
-- * Constructors
--
ctagSomeType :: forall c a. Typeable a => CTag c -> Proxy a -> SomeType
ctagSomeType TPoint a = proxySomeType (Proxy @c) a
ctagSomeType TList  a = proxySomeType (Proxy @c) a
ctagSomeType TSet   a = proxySomeType (Proxy @c) a
ctagSomeType TTree  a = proxySomeType (Proxy @c) a
ctagSomeType TDag   a = proxySomeType (Proxy @c) a
ctagSomeType TGraph a = proxySomeType (Proxy @c) a

proxySomeType ::
  forall c a
  . (ReifyCTag c, Typeable c, Typeable a)
  => Proxy c -> Proxy a -> SomeType
proxySomeType _ pa =
  CSomeType
    (Name . showSomeTypeRepNoKind $ rep)
    (typeRepTyCon $ typeRep @c)
    rep
 where rep = someTypeRep pa

someTypeFromConType :: SomeType -> SomeType -> SomeType
someTypeFromConType CSomeType{tCon} CSomeType{tName, tRep} = CSomeType{..}

unitSomeType :: SomeType
unitSomeType = ctagSomeType TPoint (Proxy @())

someType ::
  forall c a
  . (ReifyCTag c, Typeable c, Typeable a)
  => Types c a -> SomeType
someType _ = proxySomeType (Proxy @c) (Proxy @a)

--------------------------------------------------------------------------------
-- * Instances
--
instance NFData SomeType
instance Read SomeType where readPrec = failRead
instance Serialise SomeType

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

--------------------------------------------------------------------------------
showSomeTypeRepNoKind :: SomeTypeRep -> Text
showSomeTypeRepNoKind (SomeTypeRep x) = showTypeRepNoKind x

showTypeRepNoKind :: TypeRep a -> Text
showTypeRepNoKind = TB.run . flip go False
 where
   go :: TypeRep b -> Bool -> TB.Builder
   go (R.App (R.Con f) a1) _
     | f == listTyCon =
       case a1 of
         R.Con x | x == charTyCon
           -> TB.text "String"
         _ -> TB.char '[' <> go a1 False <> TB.char ']'
   go (R.App (R.App (R.Con f) a1) a2) _
     | f == tuple2TyCon =
       TB.char '(' <> go a1 False <> TB.char ',' <> TB.char ' ' <> go a2 False <> TB.char ')'
   go (R.App (R.App (R.App (R.Con f) a1) a2) a3) _
     | f == tuple3TyCon =
       TB.char '(' <> go a1 False <> TB.char ',' <> TB.char ' ' <> go a2 False <> TB.char ',' <> TB.char ' ' <> go a3 False <> TB.char ')'
   go (R.Con c) _ =
     TB.string $ show c
   go a@R.App{} True =
     TB.char '(' <> go a False <> TB.char ')'
   go (R.App f x) False =
     go f True <> TB.char ' ' <> go x True
   go f@R.Fun{} True =
     TB.char '(' <> go f False <> TB.char ')'
   go (R.Fun x r) False =
     go x True <> TB.text " -> " <> go r True

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
