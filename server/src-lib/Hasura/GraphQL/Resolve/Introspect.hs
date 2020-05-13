module Hasura.GraphQL.Resolve.Introspect
  ( schemaR
  , typeR
  ) where

import           Data.Has
import           Hasura.Prelude

import qualified Data.Aeson                         as J
import qualified Data.HashMap.Strict                as Map
import qualified Data.HashSet                       as Set
import qualified Data.Text                          as T
import qualified Language.GraphQL.Draft.Syntax      as G
import qualified Hasura.SQL.Value                   as S
import qualified Hasura.SQL.Types                   as S

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Validate.Context
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.InputValue
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types

data TypeKind
  = TKSCALAR
  | TKOBJECT
  | TKINTERFACE
  | TKUNION
  | TKENUM
  | TKINPUT_OBJECT
  | TKLIST
  | TKNON_NULL
  deriving (Show, Eq)

instance J.ToJSON TypeKind where
  toJSON = J.toJSON . T.pack . drop 2 . show

withSubFields
  :: (Monad m)
  => SelSet
  -> (Field -> m J.Value)
  -> m J.Object
withSubFields selSet fn =
  fmap Map.fromList $ forM (toList selSet) $ \fld -> do
  val <- fn fld
  return (G.unName $ G.unAlias $ _fAlias fld, val)

namedTyToTxt :: G.NamedType -> Text
namedTyToTxt = G.unName . G.unNamedType

retJ :: (Applicative m, J.ToJSON a) => a -> m J.Value
retJ = pure . J.toJSON

retJT :: (Applicative m) => Text -> m J.Value
retJT = pure . J.toJSON

-- 4.5.2.1
scalarR
  :: (Monad m)
  => ScalarTyInfo
  -> Field
  -> m J.Object
scalarR (ScalarTyInfo descM name _ _) fld =
  withSubFields (_fSelSet fld) $ \subFld ->
  case _fName subFld of
    "__typename"  -> retJT "__Type"
    "kind"        -> retJ TKSCALAR
    "description" -> retJ $ fmap G.unDescription descM
    "name"        -> retJ name
    _             -> return J.Null

-- 4.5.2.2
objectTypeR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m, MonadReusability m)
  => ObjTyInfo
  -> Field
  -> m J.Object
objectTypeR objectType fld =
  withSubFields (_fSelSet fld) $ \subFld ->
  case _fName subFld of
    "__typename"  -> retJT "__Type"
    "kind"        -> retJ TKOBJECT
    "name"        -> retJ $ namedTyToTxt n
    "description" -> retJ $ fmap G.unDescription descM
    "interfaces"  -> fmap J.toJSON $ mapM (`ifaceR` subFld) $ Set.toList iFaces
    "fields"      -> fmap J.toJSON $ mapM (`fieldR` subFld) $
                     sortOn _fiName $
                     filter notBuiltinFld $ Map.elems flds
    _             -> return J.Null
  where
    descM = _otiDesc objectType
    n = _otiName objectType
    iFaces = _otiImplIFaces objectType
    flds = _otiFields objectType

notBuiltinFld :: ObjFldInfo -> Bool
notBuiltinFld f =
  fldName /= "__typename" && fldName /= "__type" && fldName /= "__schema"
  where
    fldName = _fiName f

getImplTypes :: (MonadReader t m, Has TypeMap t) => AsObjType -> m [ObjTyInfo]
getImplTypes aot = do
   tyInfo :: TypeMap <- asks getter
   return $ sortOn _otiName $
     Map.elems $ getPossibleObjTypes' tyInfo aot

-- 4.5.2.3
unionR
  :: (MonadReader t m, MonadError QErr m, Has TypeMap t, MonadReusability m)
  => UnionTyInfo -> Field -> m J.Object
unionR u@(UnionTyInfo descM n _) fld =
  withSubFields (_fSelSet fld) $ \subFld ->
  case _fName subFld of
    "__typename"    -> retJT "__Field"
    "kind"          -> retJ TKUNION
    "name"          -> retJ $ namedTyToTxt n
    "description"   -> retJ $ fmap G.unDescription descM
    "possibleTypes" -> fmap J.toJSON $
                       mapM (`objectTypeR` subFld) =<< getImplTypes (AOTUnion u)
    _               -> return J.Null

-- 4.5.2.4
ifaceR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m, MonadReusability m)
  => G.NamedType
  -> Field
  -> m J.Object
ifaceR n fld = do
  tyInfo <- getTyInfo n
  case tyInfo of
    TIIFace ifaceTyInfo -> ifaceR' ifaceTyInfo fld
    _                   -> throw500 $ "Unknown interface " <> showNamedTy n

ifaceR'
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m, MonadReusability m)
  => IFaceTyInfo
  -> Field
  -> m J.Object
ifaceR' i@(IFaceTyInfo descM n flds) fld =
  withSubFields (_fSelSet fld) $ \subFld ->
  case _fName subFld of
    "__typename"    -> retJT "__Type"
    "kind"          -> retJ TKINTERFACE
    "name"          -> retJ $ namedTyToTxt n
    "description"   -> retJ $ fmap G.unDescription descM
    "fields"        -> fmap J.toJSON $ mapM (`fieldR` subFld) $
                      sortOn _fiName $
                      filter notBuiltinFld $ Map.elems flds
    "possibleTypes" -> fmap J.toJSON $ mapM (`objectTypeR` subFld)
                       =<< getImplTypes (AOTIFace i)
    _               -> return J.Null

-- 4.5.2.5
enumTypeR
  :: ( Monad m, MonadReusability m, MonadError QErr m )
  => EnumTyInfo
  -> Field
  -> m J.Object
enumTypeR (EnumTyInfo descM n vals _) fld =
  withSubFields (_fSelSet fld) $ \subFld ->
  case _fName subFld of
    "__typename"  -> retJT "__Type"
    "kind"        -> retJ TKENUM
    "name"        -> retJ $ namedTyToTxt n
    "description" -> retJ $ fmap G.unDescription descM
    "enumValues"  -> do
      includeDeprecated <- readIncludeDeprecated subFld
      fmap J.toJSON $
        mapM (enumValueR subFld) $
        filter (\val -> includeDeprecated || not (_eviIsDeprecated val)) $
        sortOn _eviVal $
        Map.elems (normalizeEnumValues vals)
    _             -> return J.Null

readIncludeDeprecated
  :: ( Monad m, MonadReusability m, MonadError QErr m )
  => Field
  -> m Bool
readIncludeDeprecated subFld = do
  let argM = Map.lookup "includeDeprecated" (_fArguments subFld)
  case argM of
    Nothing -> pure False
    Just arg -> asScalarVal arg S.PGBoolean >>= \case
      S.PGValBoolean b -> pure b
      _ -> throw500 "unexpected non-Boolean argument for includeDeprecated"

-- 4.5.2.6
inputObjR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m, MonadReusability m)
  => InpObjTyInfo
  -> Field
  -> m J.Object
inputObjR (InpObjTyInfo descM nt flds _) fld =
  withSubFields (_fSelSet fld) $ \subFld ->
  case _fName subFld of
    "__typename"  -> retJT "__Type"
    "kind"        -> retJ TKINPUT_OBJECT
    "name"        -> retJ $ namedTyToTxt nt
    "description" -> retJ $ fmap G.unDescription descM
    "inputFields" -> fmap J.toJSON $ mapM (inputValueR subFld) $
                     sortOn _iviName $ Map.elems flds
    _             -> return J.Null

-- 4.5.2.7
listTypeR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m, MonadReusability m)
  => G.ListType -> Field -> m J.Object
listTypeR (G.ListType ty) fld =
  withSubFields (_fSelSet fld) $ \subFld ->
  case _fName subFld of
    "__typename" -> retJT "__Type"
    "kind"       -> retJ TKLIST
    "ofType"     -> J.toJSON <$> gtypeR ty subFld
    _            -> return J.Null

-- 4.5.2.8
nonNullR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m, MonadReusability m)
  => G.GType -> Field -> m J.Object
nonNullR gTyp fld =
  withSubFields (_fSelSet fld) $ \subFld ->
  case _fName subFld of
    "__typename" -> retJT "__Type"
    "kind"       -> retJ TKNON_NULL
    "ofType"     -> case gTyp of
      G.TypeNamed (G.Nullability False) nt -> J.toJSON <$> namedTypeR nt subFld
      G.TypeList (G.Nullability False) lt  -> J.toJSON <$> listTypeR lt subFld
      _                                    -> throw500 "nullable type passed to nonNullR"
    _        -> return J.Null

namedTypeR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m, MonadReusability m)
  => G.NamedType
  -> Field
  -> m J.Object
namedTypeR nt fld = do
  tyInfo <- getTyInfo nt
  namedTypeR' fld tyInfo

namedTypeR'
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m, MonadReusability m)
  => Field
  -> TypeInfo
  -> m J.Object
namedTypeR' fld tyInfo = do
  -- Here we make sure to read the 'includeDeprecated' argument to the
  -- 'fields' and 'enumValues' fields, to ensure that the query is
  -- marked unreusable if necessary (fix #4547).
  _ <- forM (toList (_fSelSet fld)) $ \subFld ->
    case _fName subFld of
      "fields"     -> readIncludeDeprecated subFld
      "enumValues" -> readIncludeDeprecated subFld
      _            -> return False
  -- Now fetch the required type information from the corresponding
  -- information generator
  case tyInfo of
    TIScalar colTy        -> scalarR colTy fld
    TIObj objTyInfo       -> objectTypeR objTyInfo fld
    TIEnum enumTypeInfo   -> enumTypeR enumTypeInfo fld
    TIInpObj inpObjTyInfo -> inputObjR inpObjTyInfo fld
    TIIFace iFaceTyInfo   -> ifaceR' iFaceTyInfo fld
    TIUnion unionTyInfo   -> unionR unionTyInfo fld

-- 4.5.3
fieldR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m, MonadReusability m)
  => ObjFldInfo -> Field -> m J.Object
fieldR (ObjFldInfo descM n params ty _) fld =
  withSubFields (_fSelSet fld) $ \subFld ->
  case _fName subFld of
    "__typename"   -> retJT "__Field"
    "name"         -> retJ $ G.unName n
    "description"  -> retJ $ fmap G.unDescription descM
    "args"         -> fmap J.toJSON $ mapM (inputValueR subFld) $
                      sortOn _iviName $ Map.elems params
    "type"         -> J.toJSON <$> gtypeR ty subFld
    "isDeprecated" -> retJ False
    _              -> return J.Null

-- 4.5.4
inputValueR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m, MonadReusability m)
  => Field -> InpValInfo -> m J.Object
inputValueR fld (InpValInfo descM n defM ty) =
  withSubFields (_fSelSet fld) $ \subFld ->
  case _fName subFld of
    "__typename"   -> retJT "__InputValue"
    "name"         -> retJ $ G.unName n
    "description"  -> retJ $ fmap G.unDescription descM
    "type"         -> J.toJSON <$> gtypeR ty subFld
    -- TODO: figure out what the spec means by 'string encoding'
    "defaultValue" -> retJ $ pPrintValueC <$> defM
    _              -> return J.Null

-- 4.5.5
enumValueR
  :: (Monad m)
  => Field -> EnumValInfo -> m J.Object
enumValueR fld (EnumValInfo descM enumVal isDeprecated) =
  withSubFields (_fSelSet fld) $ \subFld ->
  case _fName subFld of
    "__typename"   -> retJT "__EnumValue"
    "name"         -> retJ $ G.unName $ G.unEnumValue enumVal
    "description"  -> retJ $ fmap G.unDescription descM
    "isDeprecated" -> retJ isDeprecated
    _              -> return J.Null

-- 4.5.6
directiveR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m, MonadReusability m)
  => Field -> DirectiveInfo -> m J.Object
directiveR fld (DirectiveInfo descM n args locs) =
  withSubFields (_fSelSet fld) $ \subFld ->
  case _fName subFld of
    "__typename"  -> retJT "__Directive"
    "name"        -> retJ $ G.unName n
    "description" -> retJ $ fmap G.unDescription descM
    "locations"   -> retJ $ map showDirLoc locs
    "args"        -> fmap J.toJSON $ mapM (inputValueR subFld) $
                     sortOn _iviName $ Map.elems args
    _             -> return J.Null

showDirLoc :: G.DirectiveLocation -> Text
showDirLoc = \case
  G.DLExecutable edl  -> T.pack $ drop 3 $ show edl
  G.DLTypeSystem tsdl -> T.pack $ drop 4 $ show tsdl

gtypeR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m, MonadReusability m)
  => G.GType -> Field -> m J.Object
gtypeR ty fld =
  case ty of
    G.TypeList  (G.Nullability True) lt -> listTypeR lt fld
    G.TypeList  (G.Nullability False) _ -> nonNullR ty fld
    G.TypeNamed (G.Nullability True) nt -> namedTypeR nt fld
    G.TypeNamed (G.Nullability False) _ -> nonNullR ty fld

schemaR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m, MonadReusability m)
  => Field -> m J.Object
schemaR fld =
  withSubFields (_fSelSet fld) $ \subFld -> do
  (tyMap :: TypeMap) <- asks getter
  case _fName subFld of
    "__typename"   -> retJT "__Schema"
    "types"        -> fmap J.toJSON $ mapM (namedTypeR' subFld) $
                      sortOn getNamedTy $ Map.elems tyMap
    "queryType"    -> J.toJSON <$> namedTypeR queryRootNamedType subFld
    "mutationType" -> typeR' mutationRootNamedType subFld
    "subscriptionType" -> typeR' subscriptionRootNamedType subFld
    "directives"   -> J.toJSON <$> mapM (directiveR subFld)
                      (sortOn _diName defaultDirectives)
    _              -> return J.Null

typeR
  :: (MonadReusability m, MonadError QErr m, MonadReader r m, Has TypeMap r)
  => Field -> m J.Value
typeR fld = do
  name <- asPGColText =<< getArg args "name"
  typeR' (G.NamedType $ G.Name name) fld
  where
    args = _fArguments fld

typeR'
  :: (MonadReader r m, Has TypeMap r, MonadError QErr m, MonadReusability m)
  => G.NamedType -> Field -> m J.Value
typeR' n fld = do
  tyMap <- asks getter
  case Map.lookup n tyMap of
    Nothing     -> return J.Null
    Just tyInfo -> J.Object <$> namedTypeR' fld tyInfo
