{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
module Dom.Ground (module Dom.Ground) where

import           Codec.Serialise                    (Serialise(..))
import           Data.Text                          (Text, pack)
import           Data.Typeable                      (Typeable)
import           Generics.SOP
import           Generics.SOP.Some                  (HasTypeData)
import           GHC.Generics                       (Generic)
import           Text.Read                          (Read(..))
import           Type.Reflection                    (SomeTypeRep)

import           Debug.Trace
import           Text.Printf

import qualified Data.IORef                       as IO
import qualified System.IO.Unsafe                 as IO

import Data.Dict
import Data.Parsing

import Dom.Name
import Dom.Parse
import Dom.VTag


--------------------------------------------------------------------------------
-- * Ground context
--
type     GroundCtx a =
  ( Eq             a   -- for Ord instance of SomeValueKinded
  , Ord            a   -- for Set encoding
  , Parse          a
  , Read           a
  , ReifyVTag      a
  , Serialise      a
  , Show           a
  , Typeable       a)
class    GroundCtx a => Ground a
instance GroundCtx a => Ground a

type     GroundDataCtx a =
  ( HasTypeData Ground a
  , All2 (And Typeable                Ground)  (Code a)
  , All2 (And ReifyVTag               Ground)  (Code a)
  , All2 (And Typeable (And ReifyVTag Ground)) (Code a)
  )

class    (Ground a, GroundDataCtx a) => GroundData a
instance (Ground a, GroundDataCtx a) => GroundData a

class    ( Ground a, HasTypeData Ground a
         , All2 Ground (Code a)
         ) => GroundDataFull a
instance ( Ground a, HasTypeData Ground a
         , All2 Ground (Code a)
         ) => GroundDataFull a

--------------------------------------------------------------------------------
-- * TyDict Ground
--
instance Parse (TyDict Ground) where
  parser = parseGroundDict

parseGroundDict :: (MonadFail m, TokenParsing m) => m (TyDict Ground)
parseGroundDict = do
  i <- identifier
  case lookupGroundByName i of
    Just x -> pure $ tdrTyDict x
    Nothing -> trace (printf "Unknown ground: %s" i)
                     (fail $ "Unknown ground: " <> show i)
 where
   identifier :: (Monad m, TokenParsing m) => m Text
   identifier = ident $ IdentifierStyle
     { _styleName = "Ground tag"
     , _styleStart = letter
     , _styleLetter = alphaNum
     , _styleReserved = mempty
     , _styleHighlight = Identifier
     , _styleReservedHighlight = ReservedIdentifier
     }

instance Read (TyDict Ground) where
  readPrec = do
    ty <- lexP
    case ty of
      Ident i -> case lookupGroundByName (pack i) of
        Just x -> pure $ tdrTyDict x
        Nothing -> trace (printf "Unknown ground: %s" i)
                         (fail $ "Unknown ground: " <> i)
      x -> fail $ "Unexpected construct: " <> show x

--------------------------------------------------------------------------------
-- * Ground table (TyDicts Ground): private
--
{-# NOINLINE _groundTypesRef #-}
_groundTypesRef :: IO.IORef (TyDicts Ground)
_groundTypesRef = IO.unsafePerformIO . IO.newIORef $
  error "Ground table not yet initialised."

{-# NOINLINE _groundTypes #-}
_groundTypes :: TyDicts Ground
_groundTypes = IO.unsafePerformIO $ IO.readIORef _groundTypesRef

--------------------------------------------------------------------------------
-- * Ground table (TyDicts Ground): public
--
uncurry4 :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
uncurry4 f = \(a, b, c, d) -> f a b c d

tdrIx :: TyDictsRecord c -> Int
tdrIx (x,_,_,_) = x

tdrName :: TyDictsRecord c -> Text
tdrName (_,x,_,_) = x

tdrRep :: TyDictsRecord c -> SomeTypeRep
tdrRep (_,_,x,_) = x

tdrTyDict :: TyDictsRecord c -> TyDict c
tdrTyDict (_,_,_,x) = x

{-# NOINLINE setupGroundTypes #-}
setupGroundTypes :: TyDicts Ground -> IO ()
setupGroundTypes = IO.writeIORef _groundTypesRef

{-# NOINLINE withRepGround #-}
withRepGround ::
  SomeTypeRep -> (Int -> Text -> SomeTypeRep -> TyDict Ground -> b) -> Maybe b
withRepGround k f = uncurry4 f <$> lookupByRep _groundTypes k

{-# NOINLINE withNameGround #-}
withNameGround ::
  Text -> (Int -> Text -> SomeTypeRep -> TyDict Ground -> b) -> Maybe b
withNameGround k f = uncurry4 f <$> lookupByName _groundTypes k

{-# NOINLINE lookupGroundByIx #-}
lookupGroundByIx   :: Int         -> Maybe (TyDictsRecord Ground)
lookupGroundByIx   = lookupByIx   _groundTypes

{-# NOINLINE lookupGroundByName #-}
lookupGroundByName :: Text        -> Maybe (TyDictsRecord Ground)
lookupGroundByName = lookupByName _groundTypes

{-# NOINLINE lookupGroundByRep #-}
lookupGroundByRep  :: SomeTypeRep -> Maybe (TyDictsRecord Ground)
lookupGroundByRep  = lookupByRep  _groundTypes

-- {-# NOINLINE lookupGroundNameRep #-}
-- lookupGroundNameRep :: Name VTag' -> Maybe SomeTypeRep
-- lookupGroundNameRep (Name n) = lookupRepByName _groundTypes n

{-# NOINLINE groundTypeReps #-}
groundTypeReps :: [SomeTypeRep]
groundTypeReps = tyDictReps _groundTypes

{-# NOINLINE groundTypeNames #-}
groundTypeNames :: [Text]
groundTypeNames = tyDictNames _groundTypes
