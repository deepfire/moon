module Dom.SomeValue (module Dom.SomeValue) where

import           Text.Read                          (ReadPrec)

import Basis

import Data.Parsing
import Data.Shelf

import Dom.CTag
import Dom.Cap
import Dom.Error
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
  => SV (CTag c) (SomeValueKinded c)

data SomeValueKinded (c :: Con)
  = forall a. SVK (VTag a) (Shelf Cap a) (Value c a)

mkSomeValue ::
  (Typeable c, ReifyCTag c, Ground a)
  => CTag c -> VTag a -> Caps a -> Repr c a -> SomeValue
mkSomeValue c a s =
   SV c . SVK a s . mkValue c a

-- readSomeValue :: forall (c :: Con). CTag c -> TyDict Ground -> ReadPrec SomeValue
-- readSomeValue ctag (TyDict (_ :: Proxy a)) =
--   case ctag of
--     CPoint -> do
--       v :: a <- readPrec
--       pure $ mkSomeGroundValue ctag (reifyVTag $ Proxy @a) v
--     CList -> do
--       v :: [a] <- readListPrec
--       pure $ mkSomeGroundValue ctag (reifyVTag $ Proxy @a) v
--     CSet -> do
--       v :: [a] <- readListPrec
--       pure $ mkSomeGroundValue ctag (reifyVTag $ Proxy @a) v
--     _ -> trace (printf "No parser for structures 0utside of Point/List/Set.")
--                (fail   "No parser for structures outs1de of Point/List/Set.")

--------------------------------------------------------------------------------
-- * Instances
--
instance Show SomeValue where
  show (SV _ (SVK _ s (x :: Value c a))) =
    fromMaybe
      (fromMaybe "$<SomeValue: non-Typeable>" $
         withOpenShelf s CTypeable $
           "#<SomeValueT " <> unpack (showTypeRepNoKind $ typeRep @a) <> ">") $
      showCaps s x

showCaps :: Caps a -> Value c a -> Maybe String
showCaps s x = withOpenShelf s CShow $ show x

-- instance Read SomeValue where
--   readPrec = do
--     tag :: Some CTag <- readPrec
--     dict :: TyDict Ground <- readPrec
--     case tag of
--       Exists tag' -> readSomeValue tag' dict

instance Read (Some CTag) where
  readPrec = do
    con <- lexP
    case con of
      Ident "Point" -> pure . Exists $ CPoint
      Ident "List"  -> pure . Exists $ CList
      Ident "Set"   -> pure . Exists $ CSet
      Ident "Tree"  -> pure . Exists $ CTree
      Ident "Dag"   -> pure . Exists $ CDag
      Ident "Graph" -> pure . Exists $ CGraph
      _ -> trace (printf "Unknown CTag: %s" (show con))
                 (fail "")

--------------------------------------------------------------------------------
-- * Projections
--
someValueSomeCTag :: SomeValue -> SomeCTag
someValueSomeCTag (SV t _) = SomeCTag t

someValueSomeTypeRep :: SomeValue -> SomeTypeRep
someValueSomeTypeRep (SV _ svk) = someValueKindedSomeTypeRep svk

someValueKindedSomeTypeRep :: SomeValueKinded c -> SomeTypeRep
someValueKindedSomeTypeRep (SVK _ s (_ :: Value c a)) =
  fromMaybe (error "impossible:  someValueKindedSomeTypeRep:  non-Typeable value leaked through the cracks!") $
  withOpenShelf s CTypeable $
    someTypeRep $ Proxy @a

someValueSomeType :: SomeValue -> SomeType
someValueSomeType (SV ctag (SVK _ s (_ :: Value c a))) =
  fromMaybe (error "impossible:  someValueSomeType:  non-Typeable value leaked through the cracks!") $
  withOpenShelf s CTypeable $
    ctagSomeType ctag (Proxy @a)

withSomeGroundValue ::
     SomeValue
  -> (forall c a. (ReifyCTag c, Ground a) => Value c a -> b)
  -> Maybe b
withSomeGroundValue (SV _ctag (SVK _ s sv)) f =
  withOpenShelf s CGround $
    f sv

withSomeValue ::
     Cap p
  -> SomeValue
  -> (forall c a. ReifyCTag c => Value c a -> b)
  -> Maybe b
withSomeValue cap (SV _ctag (SVK _vtag s a)) f =
  withOpenShelf s cap $ f a

withExpectedSomeValue ::
     forall a c b
   . (Typeable a, Typeable c)
  => CTag c
  -> VTag a
  -> SomeValue
  -> (Value c a -> b)
  -> Fallible b
withExpectedSomeValue _ _ sv f = case sv of
  SV (_ :: CTag c') (SVK _ s (r :: Value c' a')) ->
    fromMaybe (Left $ "withExpectedSomeValue: no Typeable") $
      withOpenShelf s CTypeable $
        case (,) (typeRep @a `eqTypeRep` typeRep @a')
                 (typeRep @c `eqTypeRep` typeRep @c') of
          (Just HRefl, Just HRefl) -> Right $ f r
          _ -> fallS $ printf "withExpectedSomeValue: expected %s/%s, got %s/%s"
               (show $ typeRep @a)  (show $ typeRep @c)
               (show $ typeRep @a') (show $ typeRep @c')

withExpectedSomeValue' ::
     forall a c b
   . (Typeable a, Typeable c)
  => CTag c
  -> Proxy a
  -> SomeValue
  -> (Value c a -> b)
  -> Fallible b
withExpectedSomeValue' _ _ sv f = case sv of
  SV (_ :: CTag c') (SVK _ s (r :: Value c' a')) ->
    fromMaybe (Left $ "withExpectedSomeValue: no Typeable") $
      withOpenShelf s CTypeable $
        case (,) (typeRep @a `eqTypeRep` typeRep @a')
                 (typeRep @c `eqTypeRep` typeRep @c') of
          (Just HRefl, Just HRefl) -> Right $ f r
          _ -> fallS $ printf "withExpectedSomeValue: expected %s/%s, got %s/%s"
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
       CPoint -> do
         v :: a <- parser
         let vtag = reifyVTag $ Proxy @a
         pure $ SV ctag $ SVK vtag capsTSG $ mkValue' a ctag v
         -- pure $ mkSomeValue ctag vtag v
       CList -> do
         v :: [a] <- commaSep parser
         let vtag = reifyVTag $ Proxy @a
         pure $ SV ctag $ SVK vtag capsTSG $ mkValue' a ctag v
         -- pure $ mkSomeValue ctag (reifyVTag $ Proxy @a) v
       CSet -> do
         v :: [a] <- commaSep parser
         let vtag = reifyVTag $ Proxy @a
         pure $ SV ctag $ SVK vtag capsTSG $ mkValue' a ctag v
         -- pure $ mkSomeValue ctag (reifyVTag $ Proxy @a) (Set.fromList v)
       _ -> trace (printf "No parser for structures outside of Point/List/Set.")
                  (fail "")

--------------------------------------------------------------------------------
-- * Encoding
--
tagSomeValue = 16--180339887

instance Serialise SomeValue where
  encode sv@(SV k (SVK _v (s :: Shelf Cap a) a)) =
    encodeListLen 3
    <> encodeWord tagSomeValue
    <> encode (SomeCTag k)
    <> repEncoding
    <> valueEncoding
   where
     (repEncoding :: Encoding, valueEncoding :: Encoding) =
       fromMaybe (error "non-Ground in Serialise",
                  error "non-Ground in Serialise") $
         withOpenShelf s CGround $
           (,) (encode $ typeRep @a)
               (encodeValue a)
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
                SV
                  <$> pure ctag
                  <*> (SVK
                       <$> pure (reifyVTag a)
                       <*> pure capsTSG
                       <*> decodeValue a ctag)
              decodeValue :: forall s (k :: Con) a
                . Ground a => Proxy a -> CTag k -> Decoder s (Value k a)
              decodeValue _ = \case
                CPoint -> VPoint <$> decode
                CList  -> VList  <$> decode
                CSet   -> VSet   <$> decode
                CTree  -> VTree  <$> decode
                CDag   -> VDag   <$> decode
                CGraph -> VGraph <$> decode
      _ -> failLenTag len tag
   where
     failLenTag :: forall s a. Typeable a => Int -> Word -> Decoder s a
     failLenTag len tag = fail $ "invalid "<>show (typeRep @a)<>" encoding: len="<>show len<>" tag="<>show tag
