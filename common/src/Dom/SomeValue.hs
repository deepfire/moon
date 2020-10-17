module Dom.SomeValue (module Dom.SomeValue) where

import           Codec.CBOR.Decoding
                   (Decoder, decodeWord, decodeListLen)
import           Codec.CBOR.Encoding
                   (Encoding, encodeWord, encodeListLen)
import           Codec.Serialise                    (Serialise(..))
import           Control.DeepSeq                    (NFData(..))
import           Data.GADT.Compare                  (GEq(..), GCompare(..), GOrdering(..))
import           Data.Foldable                      (toList)
import qualified Data.Set.Monad                   as Set
import           Data.Text                          (Text, pack)
import           Data.Typeable                      (Proxy(..), Typeable, (:~:)(..), (:~~:)(..))
import           GHC.Generics                       (Generic)
import           Text.Read                          (Lexeme(..), Read(..), ReadPrec, lexP)
import           Type.Reflection                    (SomeTypeRep, typeRep)
import qualified Type.Reflection                  as R

import           Debug.Trace
import           Text.Printf

import Data.Dict
import Data.Parsing

import Dom.CTag
import Dom.Ground
import Dom.Parse
import Dom.Some
import Dom.SomeType
import Dom.Value
import Dom.VTag


--------------------------------------------------------------------------------
-- * SomeValue
--
data SomeValue =
  forall c. (ReifyCTag c, Typeable c)
  => SomeValue       (CTag c) (SomeValueKinded c)

data SomeValueKinded (c :: Con) =
  forall a. (Ground a)
  => SomeValueKinded (VTag a) (Value c a)

-- data SomeKindValue a =
--   forall (c :: Con). Typeable c =>
--   SomeKindValue (CTag c) (Value c a)

mkSomeGroundValue ::
  (Typeable c, ReifyCTag c, Ground a)
  => CTag c -> VTag a -> Repr c a -> SomeValue
mkSomeGroundValue c a =
  SomeValue c . SomeValueKinded a . mkValue c a

readSomeValue :: forall (c :: Con). CTag c -> TyDict Ground -> ReadPrec SomeValue
readSomeValue ctag (TyDict (a :: Proxy a)) =
  case ctag of
    TPoint -> do
      v :: a <- readPrec
      pure $ mkSomeGroundValue ctag (reifyVTag $ Proxy @a) v
    TList -> do
      v :: [a] <- readListPrec
      pure $ mkSomeGroundValue ctag (reifyVTag $ Proxy @a) v
    TSet -> do
      v :: [a] <- readListPrec
      pure $ mkSomeGroundValue ctag (reifyVTag $ Proxy @a) (Set.fromList v)
    _ -> trace (printf "No parser for structures 0utside of Point/List/Set.")
               (fail   "No parser for structures outs1de of Point/List/Set.")

--------------------------------------------------------------------------------
-- * Instances
--
-- instance Eq (SomeValueKinded c) where
--   SomeValueKinded _ (va :: Value c a) == SomeValueKinded _ (vb :: Value c b) =
--     case typeRep @a `R.eqTypeRep` typeRep @b of
--       Just HRefl -> stripValue va == stripValue vb
--       Nothing -> False

-- instance Ord (SomeValueKinded c) where
--   SomeValueKinded vta (va :: Value c a) `compare`
--    SomeValueKinded vtb (vb :: Value c b) =
--     case typeRep @a `R.eqTypeRep` typeRep @b of
--       Just HRefl -> stripValue va `compare` stripValue vb
--       Nothing -> vta `compare` vtb

instance Show SomeValue where
  show (SomeValue _ (SomeValueKinded _ x)) = show x

instance Read SomeValue where
  readPrec = do
    tag :: Some CTag <- readPrec
    dict :: TyDict Ground <- readPrec
    case tag of
      Exists tag' -> readSomeValue tag' dict

instance Read (Some CTag) where
  readPrec = do
    con <- lexP
    case con of
      Ident "Point" -> pure . Exists $ TPoint
      Ident "List"  -> pure . Exists $ TList
      Ident "Set"   -> pure . Exists $ TSet
      Ident "Tree"  -> pure . Exists $ TTree
      Ident "Dag"   -> pure . Exists $ TDag
      Ident "Graph" -> pure . Exists $ TGraph
      _ -> trace (printf "Unknown CTag: %s" (show con))
                 (fail "")

--------------------------------------------------------------------------------
-- * Projections
--
someValueSomeCTag :: SomeValue -> SomeCTag
someValueSomeCTag (SomeValue t _) = SomeCTag t

someValueSomeTypeRep :: SomeValue -> R.SomeTypeRep
someValueSomeTypeRep (SomeValue _ svk) = someValueKindedSomeTypeRep svk

someValueKindedSomeTypeRep :: SomeValueKinded c -> R.SomeTypeRep
someValueKindedSomeTypeRep (SomeValueKinded _ (_ :: Value c a)) = R.someTypeRep $ Proxy @a

someValueSomeType :: SomeValue -> SomeType
someValueSomeType (SomeValue ctag (SomeValueKinded _ (_ :: Value c a))) =
  ctagSomeType ctag (Proxy @a)

withSomeValue
  :: forall a c b
   . (Typeable a, Typeable c)
  => CTag c
  -> Proxy a
  -> SomeValue
  -> (Value c a -> b)
  -> Either Text b
withSomeValue _ _ (SomeValue (_ :: CTag c') (SomeValueKinded _ (r :: Value c' a'))) f =
  case (,) (typeRep @a `R.eqTypeRep` typeRep @a')
           (typeRep @c `R.eqTypeRep` typeRep @c') of
    (Just R.HRefl, Just R.HRefl) -> Right $ f r
    _ -> Left . pack $ printf "withSomeValue: expected %s/%s, got %s/%s"
                (show $ typeRep @a)  (show $ typeRep @c)
                (show $ typeRep @a') (show $ typeRep @c')

--------------------------------------------------------------------------------
-- * Parsing
--
parseSomeValue :: Parser SomeValue -> Parser SomeValue
parseSomeValue extraParses =
  extraParses
  <|>
  braces   (parseSV . reifyCTag $ Proxy @Point)
  <|>
  brackets (parseSV . reifyCTag $ Proxy @List)
 where
   parseSV :: CTag c -> Parser SomeValue
   parseSV ctag = do
     TyDict a :: TyDict Ground <- parser
     case ctag of
       TPoint -> do
         v :: a <- parser
         let vtag = reifyVTag $ Proxy @a
         pure $ SomeValue ctag $ SomeValueKinded vtag $ mkValue' a ctag v
         -- pure $ mkSomeValue ctag vtag v
       TList -> do
         v :: [a] <- commaSep parser
         let vtag = reifyVTag $ Proxy @a
         pure $ SomeValue ctag $ SomeValueKinded vtag $ mkValue' a ctag v
         -- pure $ mkSomeValue ctag (reifyVTag $ Proxy @a) v
       TSet -> do
         v :: [a] <- commaSep parser
         let vtag = reifyVTag $ Proxy @a
         pure $ SomeValue ctag $ SomeValueKinded vtag $ mkValue' a ctag (Set.fromList v)
         -- pure $ mkSomeValue ctag (reifyVTag $ Proxy @a) (Set.fromList v)
       _ -> trace (printf "No parser for structures outside of Point/List/Set.")
                  (fail "")

--------------------------------------------------------------------------------
-- * Encoding
--
tagSomeValue = 16--180339887

instance Serialise SomeValue where
  encode sv@(SomeValue k (SomeValueKinded _vtag x)) =
    encodeListLen 3
    <> encodeWord tagSomeValue
    <> encode (SomeCTag k)
    <> encode (someValueSomeTypeRep sv)
    <> encodeValue x
   where
     encodeValue :: Ground a => Value k a -> Encoding
     encodeValue = \case
       VPoint x -> encode x
       VList  x -> encode x
       VSet   x -> encode $ toList x
       VTree  x -> encode x
       VDag   x -> encode x
       VGraph x -> encode x
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    someCTag <- decode
    case (len, tag == tagSomeValue) of
      (3, True) -> do
        str :: SomeTypeRep <- decode
        case withRepGround str (\_ _ _ x -> decodeSomeValue someCTag x) of
          Nothing -> fail $ mconcat
            ["Not a ground type: ", show str]
          Just x -> x
        where decodeSomeValue ::
                SomeCTag -> TyDict Ground -> Decoder s SomeValue
              decodeSomeValue (SomeCTag ctag) (TyDict (a :: Proxy a)) =
                SomeValue
                  <$> pure ctag
                  <*> (SomeValueKinded
                       <$> pure (reifyVTag a)
                       <*> decodeValue a ctag)
              decodeValue :: forall s (k :: Con) a
                . Ground a => Proxy a -> CTag k -> Decoder s (Value k a)
              decodeValue _ = \case
                TPoint -> VPoint <$> decode
                TList  -> VList  <$> decode
                TSet   -> VSet   <$> decode
                TTree  -> VTree  <$> decode
                TDag   -> VDag   <$> decode
                TGraph -> VGraph <$> decode
      _ -> failLenTag len tag
   where
     failLenTag :: forall s a. Typeable a => Int -> Word -> Decoder s a
     failLenTag len tag = fail $ "invalid "<>show (typeRep @a)<>" encoding: len="<>show len<>" tag="<>show tag

-- stripSomeValue ::
--   forall a c
--    . (Typeable a, Typeable c)
--   => CTag c
--   -> Proxy a
--   -> SomeValue
--   -> Maybe (Repr c a)
-- stripSomeValue _ _ (SomeValue (_ :: CTag c') (SomeValueKinded (r :: Value c' a'))) =
--   let exptr = typeRep @a
--       svtr  = typeRep @a'
--       expk  = typeRep @c
--       svk   = typeRep @c'
--   in case (,) (svtr `R.eqTypeRep` exptr)
--               (svk  `R.eqTypeRep` expk) of
--     (Just R.HRefl, Just R.HRefl) -> Just $ stripValue r
--     _ -> Nothing
