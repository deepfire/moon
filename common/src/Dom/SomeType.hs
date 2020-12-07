module Dom.SomeType (module Dom.SomeType) where

import           GHC.Generics                       (Generic)
import qualified Type.Reflection                  as Refl

import Basis

import Dom.CTag
import Dom.Name
import Dom.Tags


--------------------------------------------------------------------------------
-- * SomeType: a serialisable form of 'Type'
--
data SomeType =
  CSomeType
  { tName :: Name SomeType  -- ^ Extracted from the typerep
  , tCon  :: Refl.TyCon     -- ^ Con
  , tRep  :: SomeTypeRep    -- ^ cind.Type
  } deriving (Eq, Generic, Ord)

--------------------------------------------------------------------------------
-- * Constructors
--
ctagSomeType :: forall c a. Typeable a => CTag c -> Proxy a -> SomeType
ctagSomeType CPoint a = proxySomeType (Proxy @c) a
ctagSomeType CList  a = proxySomeType (Proxy @c) a
ctagSomeType CSet   a = proxySomeType (Proxy @c) a
ctagSomeType CTree  a = proxySomeType (Proxy @c) a
ctagSomeType CDag   a = proxySomeType (Proxy @c) a
ctagSomeType CGraph a = proxySomeType (Proxy @c) a

proxySomeType ::
  forall c a
  . (ReifyCTag c, Typeable c, Typeable a)
  => Proxy c -> Proxy a -> SomeType
proxySomeType _ pa =
  CSomeType
    (Name . showSomeTypeRepNoKind $ rep)
    (Refl.typeRepTyCon $ typeRep @c)
    rep
 where rep = someTypeRep pa

someTypeFromConType :: SomeType -> SomeType -> SomeType
someTypeFromConType CSomeType{tCon} CSomeType{tName, tRep} = CSomeType{..}

unitSomeType :: SomeType
unitSomeType = ctagSomeType CPoint (Proxy @())

typesSomeType ::
  forall c a
  . (ReifyCTag c, Typeable c, Typeable a)
  => CTagV c a -> SomeType
typesSomeType _ = proxySomeType (Proxy @c) (Proxy @a)

tagsSomeType ::
  forall c a
  . (ReifyCTag c, Typeable c, Typeable a)
  => Tags (CTagV c a) -> SomeType
tagsSomeType _ = proxySomeType (Proxy @c) (Proxy @a)

--------------------------------------------------------------------------------
-- * Instances
--
instance NFData SomeType
instance Read SomeType where readPrec = failRead
instance Serialise SomeType

instance Show SomeType where
  show (CSomeType _ tycon (SomeTypeRep rep)) =
    show tycon<>":"<>unpack (showTypeRepNoKind rep)

--------------------------------------------------------------------------------
showSomeType :: Bool -> SomeType -> Text
showSomeType showDot CSomeType{tName=(showName -> n), tCon} =
  case Refl.tyConName tCon of
    "'Point" -> if showDot then "• "<>n else n
    "'List"  -> "["<>n<>"]"
    "'Set"   -> "{"<>n<>"}"
    "'Tree"  -> "♆⇊ "<>n
    "'Dag"   -> "♆⇄ "<>n
    "'Graph" -> "☸ "<>n
    _        -> "??? "<>n
