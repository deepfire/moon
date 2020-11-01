module Dom.Pipe.IOA (module Dom.Pipe.IOA) where

import qualified Algebra.Graph                    as G
import           GHC.Generics                       (Generic)
import           Type.Reflection
                   ( pattern App
                   , pattern Con
                   , TyCon
                   , eqTypeRep
                   , someTypeRepTyCon
                   , splitApps
                   , typeRepTyCon
                   )
import Basis

import Dom.CTag
import Dom.Located
import Dom.Name
import Dom.Pipe
import Dom.Pipe.Constr
import Dom.Sig
import Dom.SomeType
import Dom.Struct
import Dom.Tags
import Dom.VTag


--------------------------------------------------------------------------------
-- * Guts of the pipe guts.
--
type family PipeFunTy (as :: [*]) (o :: *) :: * where
  PipeFunTy '[]    o = Result (ReprOf o)
  PipeFunTy (x:xs) o = ReprOf x -> PipeFunTy xs o

data IOA (c :: * -> Constraint) (as :: [*]) (o :: *) where
  IOA :: PipeConstr c as o
      => PipeFunTy as o
      -> Proxy c
      -> Proxy as
      -> Proxy o
      -> IOA c as o

pattern P
  :: Desc c as o -> Name Pipe -> Struct -> SomeTypeRep -> p
  -> [SomeType]   -> SomeType
  -> NP Tags as -> Tags o
  -> Pipe c as o p
pattern P { pDesc_, pName, pStruct, pPipeRep, pPipe, pArgStys, pOutSty, pArgs, pOut }
  <- Pipe pDesc_@(Desc pName (Sig (fmap unI -> pArgStys) (I pOutSty)) pStruct
                  pPipeRep pArgs pOut)
          pPipe

pattern IOATyCons
  :: TyCon -> TyCon -- -> TypeRep c
  -> TyCon -> TypeRep ka -> TypeRep a
  -> TypeRep rest
  -> TyCon -> TypeRep ko -> TypeRep o
  -> SomeTypeRep
pattern IOATyCons
  { ioaCon, listCon --, constrRep
  , typeACon, tagARep, aRep
  , restRep
  , typeOCon, tagORep, oRep
  }
  <- SomeTypeRep (App
                  (App (App (Con ioaCon) (Con _constrRep))
                       (App (App (Con listCon)
                                 (App (App (Con typeACon) tagARep)
                                      aRep))
                            restRep))
                  (App (App (Con typeOCon) tagORep)
                       oRep))

pattern IOATyNil
  :: TyCon -> TyCon -> TyCon -> TypeRep ko -> TypeRep o -> SomeTypeRep
pattern IOATyNil ioaCon nilCon typeOCon tagORep oRep
  <- SomeTypeRep (App
                  (App (App (Con ioaCon) (Con _cstr))
                       (Con nilCon))
                  (App (App (Con typeOCon) tagORep)
                       oRep))

--------------------------------------------------------------------------------
-- * Constructors
--
genPipe
  :: forall cf tf ct tt c
  .  ( cf ~ 'Point, tf ~ ()
     , ReifyCTag ct, ReifyVTag tt
     , Typeable ct, Typeable tt, Typeable c
     , c tt)
  => Name Pipe
  -> Types ct tt
  -> Result (Repr ct tt)
  -> Pipe c '[] (Types ct tt) Dynamic
genPipe name typ@(typesTags -> Tags cTag vTag) mv
  -- TODO: validate types against the typerep/dynamic
                = Pipe desc dyn
  where ty      = someType typ
        desc    = Desc name sig struct (dynRep dyn) Nil (Tags cTag vTag)
        sig     = Sig [] (I ty)
        struct  = Struct graph
        graph   = G.vertex ty
        dyn     = Dynamic typeRep pipeFun
        pipeFun = IOA mv Proxy Proxy Proxy ::
                  IOA c '[] (Types ct tt)

linkPipe ::
    forall cf tf ct tt c
  . ( ReifyCTag cf, ReifyCTag ct
    , ReifyVTag tf, ReifyVTag tt
    , Typeable cf, Typeable tf, Typeable ct, Typeable tt, Typeable c
    , c tt)
  => Name Pipe
  -> Types cf tf
  -> Types ct tt
  -> (Repr cf tf -> Result (Repr ct tt))
  -> Pipe c '[Types cf tf] (Types ct tt) Dynamic
linkPipe name typf@(typesTags -> Tags cf tf) typt@(typesTags -> Tags ct tt) mf
                = Pipe desc dyn
  where desc    = Desc name sig struct (dynRep dyn) (Tags cf tf :* Nil) (Tags ct tt)
        sig     = Sig [I $ someType typf] (I $ someType typt)
        struct  = Struct G.empty
        -- G.connect (G.vertex $ sIn sig) (G.vertex $ sOut sig)
        ---------
        dyn     = Dynamic typeRep pipeFun
        pipeFun = IOA mf Proxy Proxy Proxy ::
                  IOA c '[Types cf tf] (Types ct tt)

--------------------------------------------------------------------------------
-- * Utils
--
typeRepNull
  :: forall k (a :: [k]) (b :: [k])
   . (Typeable k, b ~ '[])
  => TypeRep a
  -> Maybe (a :~~: b)
typeRepNull rep = rep `eqTypeRep` typeRep @('[] :: [k])

consTyCon, ioaTyCon, nilTyCon, typeTyCon :: TyCon
consTyCon = typeRepTyCon (typeRep @(() : '[]))
nilTyCon  = someTypeRepTyCon (head $ tail $ snd $ splitApps $ typeRep @(() : '[]))
typeTyCon = typeRepTyCon (typeRep @Types)
ioaTyCon = typeRepTyCon (typeRep @IOA)

ioaTyInvalidity :: SomeTypeRep -> Maybe Text
ioaTyInvalidity (IOATyNil con lcon ocon _ko _o)
  |  con /= ioaTyCon  = Just "not an IOA"
  | lcon /= nilTyCon &&
    lcon /= consTyCon = Just ("arglist type not a list: " <> pack (show lcon))
  | ocon /= typeTyCon = Just "output not a Type"
  | otherwise = Nothing
ioaTyInvalidity _     = Just "no match with an IOA"

ioaTyConsInvalidity :: SomeTypeRep -> Maybe Text
ioaTyConsInvalidity IOATyCons{ioaCon=ioa, listCon=list, typeACon=tyA, typeOCon=tyO}
  |   ioa /= ioaTyCon  = Just "not an IOA"
  |  list /= consTyCon = Just "arglist type not a nonempty list"
  | tyA   /= typeTyCon = Just "first arg not a Type"
  | tyO   /= typeTyCon = Just "output not a Type"
  | otherwise = Nothing
ioaTyConsInvalidity _ = Just "no match with IOATyCons"

ioaTySingletonInvalidity :: SomeTypeRep -> Maybe Text
ioaTySingletonInvalidity rep@IOATyCons{}
  | Just e <- ioaTyConsInvalidity rep = Just e
  | otherwise = case rep of
      IOATyCons{restRep=Con{}} -> Nothing
      _ -> Just "arglist type not a singleton list"
ioaTySingletonInvalidity _ = Just "arglist type not a singleton list"

ioaTyNilInvalidity :: SomeTypeRep -> Maybe Text
ioaTyNilInvalidity (IOATyNil con lcon ocon _ko _o)
  |  con /= ioaTyCon  = Just "not an IOA"
  | lcon /= nilTyCon  = Just "arglist type not an empty list"
  | ocon /= typeTyCon = Just "output not a Type"
  | otherwise = Nothing
ioaTyNilInvalidity _  = Just "no match with IOATyNil"
