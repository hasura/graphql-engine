module Hasura.GraphQL.Resolve.Introspect
  ( schemaR
  , typeR
  ) where

import           Data.Has
import           Hasura.Prelude

import qualified Data.Aeson                        as J
import qualified Data.HashMap.Strict               as Map
import qualified Data.Text                         as T
import qualified Language.GraphQL.Draft.Syntax     as G

import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Validate.InputValue
import           Hasura.GraphQL.Validate.Context
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Value

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
scalarR (ScalarTyInfo descM pgColType _) fld =
  withSubFields (_fSelSet fld) $ \subFld ->
  case _fName subFld of
    "__typename"  -> retJT "__Type"
    "kind"        -> retJ TKSCALAR
    "description" -> retJ $ fmap G.unDescription descM
    "name"        -> retJ $ pgColTyToScalar pgColType
    _             -> return J.Null

-- 4.5.2.2
objectTypeR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m)
  => ObjTyInfo
  -> Field
  -> m J.Object
objectTypeR (ObjTyInfo descM n flds) fld =
  withSubFields (_fSelSet fld) $ \subFld ->
  case _fName subFld of
    "__typename"  -> retJT "__Type"
    "kind"        -> retJ TKOBJECT
    "name"        -> retJ $ namedTyToTxt n
    "description" -> retJ $ fmap G.unDescription descM
    "interfaces"  -> retJ ([] :: [()])
    "fields"      -> fmap J.toJSON $ mapM (`fieldR` subFld) $
                    sortBy (comparing _fiName) $
                    filter notBuiltinFld $ Map.elems flds
    _             -> return J.Null

notBuiltinFld :: ObjFldInfo -> Bool
notBuiltinFld f =
  fldName /= "__typename" && fldName /= "__type" && fldName /= "__schema"
  where
    fldName = _fiName f

-- 4.5.2.5
enumTypeR
  :: ( Monad m )
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
    "enumValues"  -> fmap J.toJSON $ mapM (enumValueR subFld) $
                     sortBy (comparing _eviVal) $ Map.elems vals
    _             -> return J.Null

-- 4.5.2.6
inputObjR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m)
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
                     sortBy (comparing _iviName) $ Map.elems flds
    _             -> return J.Null

-- 4.5.2.7
listTypeR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m)
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
     , MonadError QErr m)
  => G.GType -> Field -> m J.Object
nonNullR gTyp fld =
  withSubFields (_fSelSet fld) $ \subFld ->
  case _fName subFld of
    "__typename" -> retJT "__Type"
    "kind"       -> retJ TKNON_NULL
    "ofType"     -> case gTyp of
      G.TypeNamed (G.Nullability False) nt -> J.toJSON <$> namedTypeR nt subFld
      G.TypeList (G.Nullability False) lt  -> J.toJSON <$> listTypeR lt subFld
      _ -> throw500 "nullable type passed to nonNullR"
    _        -> return J.Null

namedTypeR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m)
  => G.NamedType
  -> Field
  -> m J.Object
namedTypeR nt fld = do
  tyInfo <- getTyInfo nt
  namedTypeR' fld tyInfo

namedTypeR'
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m)
  => Field
  -> TypeInfo
  -> m J.Object
namedTypeR' fld = \case
  TIScalar colTy        -> scalarR colTy fld
  TIObj objTyInfo       -> objectTypeR objTyInfo fld
  TIEnum enumTypeInfo   -> enumTypeR enumTypeInfo fld
  TIInpObj inpObjTyInfo -> inputObjR inpObjTyInfo fld

-- 4.5.3
fieldR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m)
  => ObjFldInfo -> Field -> m J.Object
fieldR (ObjFldInfo descM n params ty _) fld =
  withSubFields (_fSelSet fld) $ \subFld ->
  case _fName subFld of
    "__typename"   -> retJT "__Field"
    "name"         -> retJ $ G.unName n
    "description"  -> retJ $ fmap G.unDescription descM
    "args"         -> fmap J.toJSON $ mapM (inputValueR subFld) $
                      sortBy (comparing _iviName) $ Map.elems params
    "type"         -> J.toJSON <$> gtypeR ty subFld
    "isDeprecated" -> retJ False
    _              -> return J.Null

-- 4.5.4
inputValueR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m)
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
     , MonadError QErr m)
  => Field -> DirectiveInfo -> m J.Object
directiveR fld (DirectiveInfo descM n args locs) =
  withSubFields (_fSelSet fld) $ \subFld ->
  case _fName subFld of
    "__typename"  -> retJT "__Directive"
    "name"        -> retJ $ G.unName n
    "description" -> retJ $ fmap G.unDescription descM
    "locations"   -> retJ $ map showDirLoc locs
    "args"        -> fmap J.toJSON $ mapM (inputValueR subFld) $
                     sortBy (comparing _iviName) $ Map.elems args
    _             -> return J.Null

showDirLoc :: G.DirectiveLocation -> Text
showDirLoc = \case
  G.DLExecutable edl  -> T.pack $ drop 3 $ show edl
  G.DLTypeSystem tsdl -> T.pack $ drop 4 $ show tsdl

gtypeR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m)
  => G.GType -> Field -> m J.Object
gtypeR ty fld =
  case ty of
    G.TypeList  (G.Nullability True) lt -> listTypeR lt fld
    G.TypeList  (G.Nullability False) _ -> nonNullR ty fld
    G.TypeNamed (G.Nullability True) nt -> namedTypeR nt fld
    G.TypeNamed (G.Nullability False) _ -> nonNullR ty fld

schemaR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m)
  => Field -> m J.Object
schemaR fld =
  withSubFields (_fSelSet fld) $ \subFld -> do
  (tyMap :: TypeMap) <- asks getter
  case _fName subFld of
    "__typename"   -> retJT "__Schema"
    "types"        -> fmap J.toJSON $ mapM (namedTypeR' subFld) $
                      sortBy (comparing getNamedTy) $ Map.elems tyMap
    "queryType"    -> J.toJSON <$> namedTypeR (G.NamedType "query_root") subFld
    "mutationType" -> typeR' "mutation_root" subFld
    "subscriptionType" -> typeR' "subscription_root" subFld
    "directives"   -> J.toJSON <$> mapM (directiveR subFld)
                      (sortBy (comparing _diName) defaultDirectives)
    _              -> return J.Null

typeR
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m)
  => Field -> m J.Value
typeR fld = do
  name <- withArg args "name" $ \arg -> do
    (_, pgColVal) <- asPGColVal arg
    case pgColVal of
      PGValText t -> return t
      _           -> throw500 "expecting string for name arg of __type"
  typeR' (G.Name name) fld
  where
    args = _fArguments fld

typeR'
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m)
  => G.Name -> Field -> m J.Value
typeR' n fld = do
  tyMap <- asks getter
  case Map.lookup (G.NamedType n) tyMap of
    Nothing     -> return J.Null
    Just tyInfo -> J.Object <$> namedTypeR' fld tyInfo
