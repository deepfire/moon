{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
module Dom.Ground (module Dom.Ground) where

import           Codec.Serialise                    (Serialise(..))
import           Data.Text                          (Text, pack)
import           Data.Typeable                      (Typeable)
import           Data.Vector                        (Vector, fromList)
import           Generics.SOP
import           Generics.SOP.Some                  (HasTypeData)
import           Type.Reflection                    (SomeTypeRep)

import           Debug.Trace
import           Text.Printf

import qualified Data.IORef                       as IO
import qualified System.IO.Unsafe                 as IO

import Data.Parsing
import Data.TyDict

import Dom.Parse
import Dom.VTag


--------------------------------------------------------------------------------
-- * Ground context
--
type     GroundCtx a =
  ( -- Eq             a   -- for Ord instance of SomeValueKinded
  -- , Ord            a   -- for Set encoding
  -- ,
    Parse          a
  -- , Read           a
  , ReifyVTag      a
  , Serialise      a
  , Show           a
  , Typeable       a)
class    GroundCtx a => Ground a
instance GroundCtx a => Ground a

type     GroundDataCtx a =
  ( HasTypeData ReifyVTag a
  , All2 (And Typeable                Ground)  (Code a)
  , All2 (And ReifyVTag               Ground)  (Code a)
  , All2 (And Typeable (And ReifyVTag Ground)) (Code a)
  )

class    (Ground a, GroundDataCtx a) => GroundData a
instance (Ground a, GroundDataCtx a) => GroundData a

class    ( Ground a, HasTypeData ReifyVTag a
         , All2 Ground (Code a)
         ) => GroundDataFull a
instance ( Ground a, HasTypeData ReifyVTag a
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
{-# NOINLINE _groundTableRef #-}
_groundTableRef :: IO.IORef (TyDicts Ground)
_groundTableRef = IO.unsafePerformIO . IO.newIORef $
  error "Ground table not yet initialised."

{-# NOINLINE _groundTable #-}
_groundTable :: TyDicts Ground
_groundTable = IO.unsafePerformIO $ IO.readIORef _groundTableRef

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
setupGroundTypes = IO.writeIORef _groundTableRef

{-# NOINLINE withRepGround #-}
withRepGround ::
  SomeTypeRep -> (Int -> Text -> SomeTypeRep -> TyDict Ground -> b) -> Maybe b
withRepGround k f = uncurry4 f <$> lookupByRep _groundTable k

{-# NOINLINE withNameGround #-}
withNameGround ::
  Text -> (Int -> Text -> SomeTypeRep -> TyDict Ground -> b) -> Maybe b
withNameGround k f = uncurry4 f <$> lookupByName _groundTable k

{-# NOINLINE lookupGroundByIx #-}
lookupGroundByIx   :: Int         -> Maybe (TyDictsRecord Ground)
lookupGroundByIx   = lookupByIx   _groundTable

{-# NOINLINE lookupGroundByName #-}
lookupGroundByName :: Text        -> Maybe (TyDictsRecord Ground)
lookupGroundByName = lookupByName _groundTable

{-# NOINLINE lookupGroundByRep #-}
lookupGroundByRep  :: SomeTypeRep -> Maybe (TyDictsRecord Ground)
lookupGroundByRep  = lookupByRep  _groundTable

{-# NOINLINE groundTypeReps #-}
groundTypeReps :: [SomeTypeRep]
groundTypeReps = tyDictReps _groundTable

{-# NOINLINE groundTypeNames #-}
groundTypeNames :: Vector Text
groundTypeNames = Data.Vector.fromList $ tyDictNames _groundTable
