{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataConnector.Adapter.Metadata () where

import Control.Arrow.Extended
import Control.Monad.Trans.Control
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Environment (Environment)
import Data.Has (Has (getter))
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashMap.Strict.NonEmpty qualified as NEHashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty qualified as NEList
import Data.Map.Strict qualified as Map
import Data.Semigroup.Foldable (Foldable1 (..))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text.Extended (toTxt, (<<>), (<>>))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.API.V0 (FunctionInfo (_fiDescription, _fiName))
import Hasura.Backends.DataConnector.API.V0.Table qualified as DC (TableType (..))
import Hasura.Backends.DataConnector.Adapter.Backend (columnTypeToScalarType)
import Hasura.Backends.DataConnector.Adapter.ConfigTransform (transformSourceConfig, validateConnSourceConfig)
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC
import Hasura.Backends.DataConnector.Agent.Client (AgentClientContext (..), runAgentClientT)
import Hasura.Backends.DataConnector.Agent.Client qualified as Client
import Hasura.Backends.Postgres.SQL.Types (PGDescription (..))
import Hasura.Base.Error (Code (..), QErr (..), decodeValue, runAesonParser, throw400, withPathK)
import Hasura.Function.Cache
  ( FunctionConfig (..),
    FunctionExposedAs (FEAMutation, FEAQuery),
    FunctionInfo (..),
    FunctionOverloads (FunctionOverloads),
    FunctionPermissionsMap,
    FunctionVolatility (FTSTABLE, FTVOLATILE),
    InputArgument (..),
    TrackableFunctionInfo (..),
    TrackableInfo (..),
    TrackableTableInfo (..),
    getFuncArgNameTxt,
  )
import Hasura.Function.Common
  ( getFunctionAggregateGQLName,
    getFunctionArgsGQLName,
    getFunctionGQLName,
  )
import Hasura.Incremental qualified as Inc
import Hasura.Incremental.Select qualified as Inc
import Hasura.Logging (Hasura, Logger)
import Hasura.LogicalModel.Cache
import Hasura.LogicalModel.Metadata (LogicalModelMetadata (..))
import Hasura.LogicalModel.Types
import Hasura.NativeQuery.InterpolatedQuery (trimQueryEnd)
import Hasura.NativeQuery.Metadata (NativeQueryMetadata (..))
import Hasura.NativeQuery.Validation
import Hasura.Prelude
import Hasura.RQL.DDL.Relationship (defaultBuildArrayRelationshipInfo, defaultBuildObjectRelationshipInfo)
import Hasura.RQL.IR.BoolExp (ComparisonNullability (..), OpExpG (..), PartialSQLExp (..), RootOrCurrent (..), RootOrCurrentColumn (..))
import Hasura.RQL.Types.Backend (FunctionReturnType (..), functionGraphQLName)
import Hasura.RQL.Types.BackendType (BackendSourceKind (..), BackendType (..))
import Hasura.RQL.Types.Column qualified as RQL.T.C
import Hasura.RQL.Types.Common (JsonAggSelect (JASMultipleRows, JASSingleObject), OID (..), SourceName, SystemDefined)
import Hasura.RQL.Types.EventTrigger (RecreateEventTriggers (RETDoNothing))
import Hasura.RQL.Types.Metadata (SourceMetadata (..))
import Hasura.RQL.Types.Metadata.Backend (BackendMetadata (..))
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.NamingCase (NamingCase)
import Hasura.RQL.Types.Relationships.Local (ArrRelDef, ObjRelDef, RelInfo (), RelMapping (..))
import Hasura.RQL.Types.SchemaCache (CacheRM, askSourceConfig, askSourceInfo)
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.SchemaCacheTypes (DependencyReason (DRTable), SchemaDependency (SchemaDependency), SchemaObjId (SOSourceObj), SourceObjId (SOITable))
import Hasura.RQL.Types.Source (DBObjectsIntrospection (..), SourceInfo (..))
import Hasura.RQL.Types.Source.Column (ColumnValueGenerationStrategy (..), SourceColumnInfo (..))
import Hasura.RQL.Types.Source.Table (SourceConstraint (..), SourceForeignKeys (..), SourceTableInfo (..), SourceTableType (..))
import Hasura.RQL.Types.SourceCustomization (applyFieldNameCaseCust)
import Hasura.SQL.AnyBackend (mkAnyBackend)
import Hasura.SQL.Types (CollectableType (..))
import Hasura.Server.Migrate.Version (SourceCatalogMigrationState (..))
import Hasura.Server.Utils qualified as HSU
import Hasura.Services.Network
import Hasura.Session (SessionVariable, mkSessionVariable)
import Hasura.Table.Cache (ForeignKey (_fkConstraint))
import Hasura.Table.Cache qualified as RQL.T.T
import Hasura.Tracing (ignoreTraceT)
import Language.GraphQL.Draft.Syntax qualified as G
import Language.GraphQL.Draft.Syntax qualified as GQL
import Network.HTTP.Client qualified as HTTP
import Witch qualified

instance BackendMetadata 'DataConnector where
  prepareCatalog _ = pure (RETDoNothing, SCMSNotSupported)
  type BackendInvalidationKeys 'DataConnector = HashMap DC.DataConnectorName Inc.InvalidationKey
  resolveBackendInfo = resolveBackendInfo'
  resolveSourceConfig = resolveSourceConfig'
  resolveDatabaseMetadata = resolveDatabaseMetadata'
  parseBoolExpOperations = parseBoolExpOperations'
  parseCollectableType = parseCollectableType'
  buildComputedFieldInfo = error "buildComputedFieldInfo: not implemented for the Data Connector backend."
  buildArrayRelationshipInfo = buildArrayRelationshipInfo'
  buildObjectRelationshipInfo = buildObjectRelationshipInfo'

  -- If/when we implement enums for Data Connector backend, we will also need to fix columnTypeToScalarType function
  -- in Hasura.Backends.DataConnector.Adapter.Backend. See note there for more information.
  fetchAndValidateEnumValues = error "fetchAndValidateEnumValues: not implemented for the Data Connector backend."
  buildFunctionInfo = buildFunctionInfo'
  updateColumnInEventTrigger = error "updateColumnInEventTrigger: not implemented for the Data Connector backend."
  postDropSourceHook _sourceConfig _tableTriggerMap = pure ()
  buildComputedFieldBooleanExp _ _ _ _ _ _ =
    error "buildComputedFieldBooleanExp: not implemented for the Data Connector backend."
  listAllTables = listAllTables'
  listAllTrackables = listAllTrackables'
  getTableInfo = getTableInfo'
  supportsBeingRemoteRelationshipTarget = supportsBeingRemoteRelationshipTarget'

  validateNativeQuery _ _ _ sc _ nq = do
    unless (isJust (API._cInterpolatedQueries (DC._scCapabilities sc))) do
      let nqName = _nqmRootFieldName nq
      throw400 NotSupported $ "validateNativeQuery: " <> toTxt nqName <> " - Native Queries not implemented for this Data Connector backend."
    -- Adapted from server/src-lib/Hasura/Backends/BigQuery/Instances/Metadata.hs
    validateArgumentDeclaration nq
    pure (trimQueryEnd (_nqmCode nq)) -- for now, all queries are valid

arityJsonAggSelect :: API.FunctionArity -> JsonAggSelect
arityJsonAggSelect = \case
  API.FunctionArityOne -> JASSingleObject
  API.FunctionArityMany -> JASMultipleRows

functionReturnTypeFromAPI ::
  (MonadError QErr m) =>
  DC.FunctionName ->
  (Maybe (FunctionReturnType 'DataConnector), Maybe API.FunctionReturnType) ->
  m DC.TableName
functionReturnTypeFromAPI funcGivenName = \case
  (Just (DC.FunctionReturnsTable t), _) -> pure t
  (_, Just (API.FunctionReturnsTable t)) -> pure (Witch.into t)
  _ ->
    throw400 NotSupported
      $ "Function "
      <> toTxt funcGivenName
      <> " is missing a return type - This should be explicit in metadata, or inferred from agent"

buildFunctionInfo' ::
  (MonadError QErr m) =>
  SourceName ->
  DC.FunctionName ->
  SystemDefined ->
  FunctionConfig 'DataConnector ->
  FunctionPermissionsMap ->
  API.FunctionInfo ->
  Maybe Text ->
  NamingCase ->
  m
    ( Hasura.Function.Cache.FunctionInfo 'DataConnector,
      SchemaDependency
    )
buildFunctionInfo'
  sourceName
  funcName
  sysDefined
  funcConfig@FunctionConfig {..}
  permissionMap
  (API.FunctionInfo infoName infoType returnType infoSet infoArgs infoDesc)
  funcComment
  namingCase =
    do
      funcGivenName <- functionGraphQLName @'DataConnector funcName `onLeft` throwError
      let (volitility, exposeAs) = case infoType of
            API.FRead -> (FTSTABLE, FEAQuery)
            API.FWrite -> (FTVOLATILE, FEAMutation)
          setNamingCase = applyFieldNameCaseCust namingCase
      objid <-
        case (_fcResponse, returnType) of
          (Just (DC.FunctionReturnsTable t), _) -> pure $ SOSourceObj sourceName $ mkAnyBackend $ SOITable @'DataConnector t
          (_, Just (API.FunctionReturnsTable t)) -> pure $ SOSourceObj sourceName $ mkAnyBackend $ SOITable @'DataConnector (Witch.into t)
          _ ->
            throw400 NotSupported
              $ "Function "
              <> tshow funcName
              <> " is missing a return type - This should be explicit in metadata, or inferred from agent"

      inputArguments <- do
        let argNames = map API._faInputArgName infoArgs
            invalidArgs = filter (isNothing . GQL.mkName) argNames
        unless (null invalidArgs) $ throw400 NotSupported $ "Invalid argument names: " <> tshow invalidArgs
        -- Modified version of makeInputArguments from PG:
        case _fcSessionArgument of
          Nothing -> pure $ Seq.fromList $ map IAUserProvided infoArgs
          Just sessionArgName -> do
            unless (any (\arg -> getFuncArgNameTxt sessionArgName == API._faInputArgName arg) infoArgs)
              $ throw400 NotSupported
              $ "Session argument not mappable: "
              <> tshow sessionArgName
            pure
              $ Seq.fromList
              $ flip map infoArgs
              $ \arg ->
                if getFuncArgNameTxt sessionArgName == API._faInputArgName arg
                  then IASessionVariables sessionArgName
                  else IAUserProvided arg

      functionReturnType <- functionReturnTypeFromAPI funcName (_fcResponse, returnType)
      jsonAggSelect <-
        arityJsonAggSelect
          <$> infoSet
          `onNothing` throw400 NotSupported ("Function " <> tshow funcName <> " is missing a response cardinality")

      let funcInfo =
            FunctionInfo
              { _fiSQLName = Witch.into infoName, -- Converts to DC.FunctionName
                _fiGQLName = getFunctionGQLName funcGivenName funcConfig setNamingCase,
                _fiGQLArgsName = getFunctionArgsGQLName funcGivenName funcConfig setNamingCase,
                _fiGQLAggregateName = getFunctionAggregateGQLName funcGivenName funcConfig setNamingCase,
                _fiSystemDefined = sysDefined,
                _fiVolatility = volitility,
                _fiExposedAs = exposeAs,
                _fiInputArgs = inputArguments,
                _fiReturnType = functionReturnType,
                _fiDescription = infoDesc,
                _fiPermissions = permissionMap,
                _fiJsonAggSelect = jsonAggSelect,
                _fiComment = funcComment
              }
      pure $ (funcInfo, SchemaDependency objid DRTable)

resolveBackendInfo' ::
  forall arr m.
  ( ArrowChoice arr,
    Inc.ArrowCache m arr,
    Inc.ArrowDistribute arr,
    ArrowWriter (Seq CollectItem) arr,
    MonadIO m,
    MonadBaseControl IO m,
    ProvidesNetwork m
  ) =>
  Logger Hasura ->
  (Inc.Dependency (Maybe (HashMap DC.DataConnectorName Inc.InvalidationKey)), Map.Map DC.DataConnectorName DC.DataConnectorOptions) `arr` HashMap DC.DataConnectorName DC.DataConnectorInfo
resolveBackendInfo' logger = proc (invalidationKeys, optionsMap) -> do
  maybeDataConnectorCapabilities <-
    (|
      Inc.keyed
        ( \dataConnectorName dataConnectorOptions -> do
            getDataConnectorCapabilitiesIfNeeded -< (invalidationKeys, dataConnectorName, dataConnectorOptions)
        )
      |)
      (toHashMap optionsMap)
  returnA -< HashMap.catMaybes maybeDataConnectorCapabilities
  where
    getDataConnectorCapabilitiesIfNeeded ::
      (Inc.Dependency (Maybe (HashMap DC.DataConnectorName Inc.InvalidationKey)), DC.DataConnectorName, DC.DataConnectorOptions) `arr` Maybe DC.DataConnectorInfo
    getDataConnectorCapabilitiesIfNeeded = Inc.cache proc (invalidationKeys, dataConnectorName, dataConnectorOptions) -> do
      let metadataObj = MetadataObject (MODataConnectorAgent dataConnectorName) $ J.toJSON dataConnectorName
      httpMgr <- bindA -< askHTTPManager
      Inc.dependOn -< Inc.selectMaybeD (Inc.ConstS dataConnectorName) invalidationKeys
      maybeDcInfo <- (| withRecordInconsistency (bindErrorA -< getDataConnectorCapabilities dataConnectorOptions httpMgr) |) metadataObj
      returnA -< join maybeDcInfo

    getDataConnectorCapabilities ::
      DC.DataConnectorOptions ->
      HTTP.Manager ->
      ExceptT QErr m (Maybe DC.DataConnectorInfo)
    getDataConnectorCapabilities options@DC.DataConnectorOptions {..} manager =
      ( ignoreTraceT
          . flip runAgentClientT (AgentClientContext logger _dcoUri manager Nothing Nothing)
          $ (Just . mkDataConnectorInfo options)
          <$> Client.capabilities
      )
        `catchError` ignoreConnectionErrors

    -- If we can't connect to a data connector agent to get its capabilities
    -- we don't throw an error, we just return Nothing, which means the agent is in a broken state
    -- but we don't fail the schema cache building process with metadata inconsistencies if the
    -- agent isn't in use. If the agent is in use, when we go to consume its 'DC.DataConnectorInfo'
    -- later, it will be missing and we'll throw an error then.
    ignoreConnectionErrors :: QErr -> ExceptT QErr m (Maybe a)
    ignoreConnectionErrors err@QErr {..} =
      if qeCode == ConnectionNotEstablished
        then pure Nothing
        else throwError err

    mkDataConnectorInfo :: DC.DataConnectorOptions -> API.CapabilitiesResponse -> DC.DataConnectorInfo
    mkDataConnectorInfo options API.CapabilitiesResponse {..} =
      DC.DataConnectorInfo options _crCapabilities _crConfigSchemaResponse _crDisplayName _crReleaseName

    toHashMap = HashMap.fromList . Map.toList

resolveSourceConfig' ::
  (MonadIO m) =>
  SourceName ->
  DC.ConnSourceConfig ->
  BackendSourceKind 'DataConnector ->
  HashMap DC.DataConnectorName DC.DataConnectorInfo ->
  Environment ->
  HTTP.Manager ->
  m (Either QErr DC.SourceConfig)
resolveSourceConfig'
  sourceName
  csc@DC.ConnSourceConfig {_cscTemplate, _cscTemplateVariables, _cscTimeout, _cscValue = originalConfig}
  (DataConnectorKind dataConnectorName)
  backendInfo
  env
  manager = runExceptT do
    DC.DataConnectorInfo {_dciOptions = DC.DataConnectorOptions {_dcoUri}, ..} <- getDataConnectorInfo dataConnectorName backendInfo

    validateConnSourceConfig dataConnectorName sourceName _dciConfigSchemaResponse csc Nothing env

    pure
      DC.SourceConfig
        { _scEndpoint = _dcoUri,
          _scConfig = originalConfig,
          _scTemplate = _cscTemplate,
          _scTemplateVariables = fromMaybe mempty _cscTemplateVariables,
          _scCapabilities = _dciCapabilities,
          _scManager = manager,
          _scTimeoutMicroseconds = (DC.sourceTimeoutMicroseconds <$> _cscTimeout),
          _scDataConnectorName = dataConnectorName,
          _scEnvironment = env
        }

getDataConnectorInfo :: (MonadError QErr m) => DC.DataConnectorName -> HashMap DC.DataConnectorName DC.DataConnectorInfo -> m DC.DataConnectorInfo
getDataConnectorInfo dataConnectorName backendInfo =
  onNothing (HashMap.lookup dataConnectorName backendInfo)
    $ throw400 DataConnectorError ("Data connector named " <> toTxt dataConnectorName <<> " was not found in the data connector backend info")

mkRawColumnType :: API.ColumnType -> RQL.T.C.RawColumnType 'DataConnector
mkRawColumnType = \case
  API.ColumnTypeScalar scalarType -> RQL.T.C.RawColumnTypeScalar $ Witch.from scalarType
  API.ColumnTypeObject name -> RQL.T.C.RawColumnTypeObject () name
  API.ColumnTypeArray columnType isNullable -> RQL.T.C.RawColumnTypeArray () (mkRawColumnType columnType) isNullable

resolveDatabaseMetadata' ::
  ( MonadIO m,
    MonadBaseControl IO m
  ) =>
  Logger Hasura ->
  SourceMetadata 'DataConnector ->
  DC.SourceConfig ->
  m (Either QErr (DBObjectsIntrospection 'DataConnector))
resolveDatabaseMetadata' logger sourceMetadata@SourceMetadata {_smName} sourceConfig = runExceptT do
  API.SchemaResponse {..} <-
    if supportsSchemaPost' sourceConfig
      then do
        let schemaRequest = makeTrackedItemsOnlySchemaRequest sourceMetadata
        requestDatabaseSchemaPost logger _smName sourceConfig schemaRequest
      else requestDatabaseSchemaGet logger _smName sourceConfig
  let logicalModels =
        maybe mempty (InsOrdHashMap.fromList . map toLogicalModelMetadata . toList) _srObjectTypes
      tables = HashMap.fromList $ do
        API.TableInfo {..} <- _srTables
        let primaryKeyColumns = fmap Witch.from . NESeq.fromList <$> _tiPrimaryKey
        let meta =
              RQL.T.T.DBTableMetadata
                { _ptmiOid = OID 0, -- TODO: This is wrong and needs to be fixed. It is used for diffing tables and seeing what's new/deleted/altered, so reusing 0 for all tables is problematic.
                  _ptmiColumns = do
                    API.ColumnInfo {..} <- _tiColumns
                    pure
                      $ RQL.T.C.RawColumnInfo
                        { rciName = Witch.from _ciName,
                          rciPosition = 1, -- TODO: This is very wrong and needs to be fixed. It is used for diffing tables and seeing what's new/deleted/altered, so reusing 1 for all columns is problematic.
                          rciType = mkRawColumnType _ciType,
                          rciIsNullable = _ciNullable,
                          rciDescription = fmap GQL.Description _ciDescription,
                          rciMutability = RQL.T.C.ColumnMutability _ciInsertable _ciUpdatable
                        },
                  _ptmiPrimaryKey = RQL.T.T.PrimaryKey (RQL.T.T.Constraint (DC.ConstraintName "") (OID 0)) <$> primaryKeyColumns,
                  _ptmiUniqueConstraints = mempty,
                  _ptmiForeignKeys = buildForeignKeySet _tiForeignKeys,
                  _ptmiViewInfo =
                    ( if _tiType == API.Table && _tiInsertable && _tiUpdatable && _tiDeletable
                        then Nothing
                        else Just $ RQL.T.T.ViewInfo _tiInsertable _tiUpdatable _tiDeletable
                    ),
                  _ptmiDescription = fmap PGDescription _tiDescription,
                  _ptmiExtraTableMetadata =
                    DC.ExtraTableMetadata
                      { _etmTableType = _tiType,
                        _etmExtraColumnMetadata =
                          _tiColumns
                            & fmap (\API.ColumnInfo {..} -> (Witch.from _ciName, DC.ExtraColumnMetadata _ciValueGenerated))
                            & HashMap.fromList
                      }
                }
        pure (Witch.into _tiName, meta)
      functions =
        let sorted = sortOn _fiName _srFunctions
            grouped = NEList.groupBy ((==) `on` _fiName) sorted
         in HashMap.fromList do
              infos@(API.FunctionInfo {..} NEList.:| _) <- grouped
              pure (Witch.into _fiName, FunctionOverloads infos)
   in pure
        DBObjectsIntrospection
          { _rsTables = tables,
            _rsFunctions = functions,
            _rsScalars = mempty,
            _rsLogicalModels = logicalModels
          }

makeTrackedItemsOnlySchemaRequest :: SourceMetadata 'DataConnector -> API.SchemaRequest
makeTrackedItemsOnlySchemaRequest SourceMetadata {..} =
  API.SchemaRequest
    { _srFilters =
        API.SchemaFilters
          { _sfOnlyTables = Just $ Witch.into <$> InsOrdHashMap.keys _smTables,
            _sfOnlyFunctions = Just $ Witch.into <$> InsOrdHashMap.keys _smFunctions
          },
      _srDetailLevel = API.Everything
    }

requestDatabaseSchemaGet ::
  (MonadIO m, MonadBaseControl IO m, MonadError QErr m) =>
  Logger Hasura ->
  SourceName ->
  DC.SourceConfig ->
  m API.SchemaResponse
requestDatabaseSchemaGet logger sourceName sourceConfig = do
  transformedSourceConfig <- transformSourceConfig sourceConfig Nothing
  ignoreTraceT
    . flip runAgentClientT (AgentClientContext logger (DC._scEndpoint transformedSourceConfig) (DC._scManager transformedSourceConfig) (DC._scTimeoutMicroseconds transformedSourceConfig) Nothing)
    $ Client.schemaGet sourceName (DC._scConfig transformedSourceConfig)

requestDatabaseSchemaPost ::
  (MonadIO m, MonadBaseControl IO m, MonadError QErr m) =>
  Logger Hasura ->
  SourceName ->
  DC.SourceConfig ->
  API.SchemaRequest ->
  m API.SchemaResponse
requestDatabaseSchemaPost logger sourceName sourceConfig schemaRequest = do
  transformedSourceConfig <- transformSourceConfig sourceConfig Nothing
  ignoreTraceT
    . flip runAgentClientT (AgentClientContext logger (DC._scEndpoint transformedSourceConfig) (DC._scManager transformedSourceConfig) (DC._scTimeoutMicroseconds transformedSourceConfig) Nothing)
    $ Client.schemaPost sourceName (DC._scConfig transformedSourceConfig) schemaRequest

getFieldType :: Bool -> API.ColumnType -> LogicalModelType 'DataConnector
getFieldType isNullable = \case
  API.ColumnTypeScalar scalarType -> LogicalModelTypeScalar $ LogicalModelTypeScalarC (Witch.from scalarType) isNullable
  API.ColumnTypeObject objectTypeName -> LogicalModelTypeReference $ LogicalModelTypeReferenceC (LogicalModelName objectTypeName) isNullable
  API.ColumnTypeArray columnType isNullable' -> LogicalModelTypeArray $ LogicalModelTypeArrayC (getFieldType isNullable' columnType) isNullable

toLogicalModelMetadata :: API.ObjectTypeDefinition -> (LogicalModelName, LogicalModelMetadata 'DataConnector)
toLogicalModelMetadata API.ObjectTypeDefinition {..} =
  ( logicalModelName,
    LogicalModelMetadata
      { _lmmName = logicalModelName,
        _lmmFields = InsOrdHashMap.fromList $ toList $ toTableObjectFieldDefinition <$> _otdColumns,
        _lmmDescription = _otdDescription,
        _lmmSelectPermissions = mempty
      }
  )
  where
    logicalModelName = LogicalModelName _otdName
    toTableObjectFieldDefinition API.ColumnInfo {..} =
      let fieldType = getFieldType _ciNullable _ciType
          columnName = Witch.from _ciName
       in ( columnName,
            LogicalModelField
              { lmfName = columnName,
                lmfType = fieldType,
                lmfDescription = _ciDescription
              }
          )

-- | Construct a 'HashSet' 'RQL.T.T.ForeignKeyMetadata'
-- 'DataConnector' to build the foreign key constraints in the table
-- metadata.
buildForeignKeySet :: API.ForeignKeys -> HashSet (RQL.T.T.ForeignKeyMetadata 'DataConnector)
buildForeignKeySet (API.ForeignKeys constraints) =
  HashSet.fromList
    $ constraints
    & HashMap.foldMapWithKey @[RQL.T.T.ForeignKeyMetadata 'DataConnector]
      \constraintName API.Constraint {..} -> maybeToList do
        let columnMapAssocList = HashMap.foldrWithKey' (\k v acc -> (Witch.from k, Witch.from v) : acc) [] $ API.unColumnPathMapping _cColumnMapping
        columnMapping <- NEHashMap.fromList columnMapAssocList
        let foreignKey =
              RQL.T.T.ForeignKey
                { _fkConstraint = RQL.T.T.Constraint (Witch.from constraintName) (OID 1),
                  _fkForeignTable = Witch.from _cForeignTable,
                  _fkColumnMapping = columnMapping
                }
        pure $ RQL.T.T.ForeignKeyMetadata foreignKey

-- | This is needed to get permissions to work
parseBoolExpOperations' ::
  forall m v.
  (MonadError QErr m) =>
  RQL.T.C.ValueParser 'DataConnector m v ->
  RQL.T.T.FieldInfoMap (RQL.T.T.FieldInfo 'DataConnector) ->
  RQL.T.T.FieldInfoMap (RQL.T.T.FieldInfo 'DataConnector) ->
  RQL.T.C.ColumnReference 'DataConnector ->
  J.Value ->
  m [OpExpG 'DataConnector v]
parseBoolExpOperations' rhsParser rootFieldInfoMap fieldInfoMap columnRef value =
  withPathK (toTxt columnRef) $ parseOperations value
  where
    columnType :: RQL.T.C.ColumnType 'DataConnector
    columnType = RQL.T.C.columnReferenceType columnRef

    parseWithTy ty = rhsParser (CollectableTypeScalar ty)

    parseOperations :: J.Value -> m [OpExpG 'DataConnector v]
    parseOperations = \case
      J.Object o -> traverse (parseOperation . first K.toText) $ KM.toList o
      v -> pure . AEQ NullableComparison <$> parseWithTy columnType v

    parseOperation :: (Text, J.Value) -> m (OpExpG 'DataConnector v)
    parseOperation (opStr, val) = withPathK opStr
      $ case opStr of
        "_eq" -> parseEq
        "$eq" -> parseEq
        "_neq" -> parseNeq
        "$neq" -> parseNeq
        "_gt" -> parseGt
        "$gt" -> parseGt
        "_lt" -> parseLt
        "$lt" -> parseLt
        "_gte" -> parseGte
        "$gte" -> parseGte
        "_lte" -> parseLte
        "$lte" -> parseLte
        "_in" -> parseIn
        "$in" -> parseIn
        "_nin" -> parseNin
        "$nin" -> parseNin
        "_is_null" -> parseIsNull
        "$is_null" -> parseIsNull
        "_ceq" -> parseCeq
        "$ceq" -> parseCeq
        "_cneq" -> parseCne
        "$cneq" -> parseCne
        "_cgt" -> parseCgt
        "$cgt" -> parseCgt
        "_clt" -> parseClt
        "$clt" -> parseClt
        "_cgte" -> parseCgte
        "$cgte" -> parseCgte
        "_clte" -> parseClte
        "$clte" -> parseClte
        -- "_like"          -> parseLike
        -- "$like"          -> parseLike
        --
        -- "_nlike"         -> parseNlike
        -- "$nlike"         -> parseNlike
        --
        -- "_cast" -> parseCast
        -- "$cast" -> parseCast

        x -> throw400 UnexpectedPayload $ "Unknown operator: " <> x
      where
        parseOne = parseWithTy columnType val
        parseManyWithType ty = rhsParser (CollectableTypeArray ty) val

        parseEq = AEQ NullableComparison <$> parseOne
        parseNeq = ANE NullableComparison <$> parseOne
        parseIn = AIN <$> parseManyWithType columnType
        parseNin = ANIN <$> parseManyWithType columnType
        parseGt = AGT <$> parseOne
        parseLt = ALT <$> parseOne
        parseGte = AGTE <$> parseOne
        parseLte = ALTE <$> parseOne
        parseIsNull = bool ANISNOTNULL ANISNULL <$> decodeValue val
        parseCeq = CEQ <$> decodeAndValidateRhsCol val
        parseCne = CNE <$> decodeAndValidateRhsCol val
        parseCgt = CGT <$> decodeAndValidateRhsCol val
        parseClt = CLT <$> decodeAndValidateRhsCol val
        parseCgte = CGTE <$> decodeAndValidateRhsCol val
        parseClte = CLTE <$> decodeAndValidateRhsCol val

        decodeAndValidateRhsCol :: J.Value -> m (RootOrCurrentColumn 'DataConnector)
        decodeAndValidateRhsCol v = case v of
          J.String _ -> go IsCurrent fieldInfoMap v
          J.Array path -> case toList path of
            [] -> throw400 Unexpected "path cannot be empty"
            [col] -> go IsCurrent fieldInfoMap col
            [J.String "$", col] -> go IsRoot rootFieldInfoMap col
            _ -> throw400 NotSupported "Relationship references are not supported in column comparison RHS"
          _ -> throw400 Unexpected "a boolean expression JSON must be either a string or an array"
          where
            go rootInfo fieldInfoMap' columnValue = do
              colName <- decodeValue columnValue
              colInfo <- validateRhsColumn fieldInfoMap' colName
              pure $ RootOrCurrentColumn rootInfo colInfo

        validateRhsColumn :: RQL.T.T.FieldInfoMap (RQL.T.T.FieldInfo 'DataConnector) -> DC.ColumnName -> m DC.ColumnName
        validateRhsColumn fieldInfoMap' rhsCol = do
          rhsType <- RQL.T.T.askColumnType fieldInfoMap' rhsCol "column operators can only compare table columns"
          when (columnType /= rhsType)
            $ throw400 UnexpectedPayload
            $ "incompatible column types: "
            <> columnRef
            <<> " has type "
            <> columnType
            <<> ", but "
            <> rhsCol
            <<> " has type "
            <>> rhsType
          pure rhsCol

parseCollectableType' ::
  (MonadError QErr m, MonadReader r m, Has API.ScalarTypesCapabilities r) =>
  CollectableType (RQL.T.C.ColumnType 'DataConnector) ->
  J.Value ->
  m (PartialSQLExp 'DataConnector)
parseCollectableType' collectableType = \case
  J.String t
    | HSU.isSessionVariable t -> pure $ mkTypedSessionVar collectableType $ mkSessionVariable t
    | HSU.isReqUserId t -> pure $ mkTypedSessionVar collectableType HSU.userIdHeader
  val -> case collectableType of
    CollectableTypeScalar columnType ->
      PSESQLExp . DC.ValueLiteral (columnTypeToScalarType columnType) <$> RQL.T.C.parseScalarValueColumnType columnType val
    CollectableTypeArray columnType -> do
      vals <- runAesonParser J.parseJSON val
      scalarValues <- RQL.T.C.parseScalarValuesColumnType columnType vals
      pure . PSESQLExp $ DC.ArrayLiteral (columnTypeToScalarType columnType) scalarValues

mkTypedSessionVar ::
  CollectableType (RQL.T.C.ColumnType 'DataConnector) ->
  SessionVariable ->
  PartialSQLExp 'DataConnector
mkTypedSessionVar columnType =
  PSESessVar (columnTypeToScalarType <$> columnType)

buildObjectRelationshipInfo' ::
  (MonadError QErr m) =>
  DC.SourceConfig ->
  SourceName ->
  HashMap DC.TableName (HashSet (ForeignKey 'DataConnector)) ->
  DC.TableName ->
  ObjRelDef 'DataConnector ->
  m (RelInfo 'DataConnector, Seq SchemaDependency)
buildObjectRelationshipInfo' sourceConfig sourceName fks tableName objRel = do
  ifSupportsLocalRelationships sourceName sourceConfig
    $ defaultBuildObjectRelationshipInfo sourceName fks tableName objRel

buildArrayRelationshipInfo' ::
  (MonadError QErr m) =>
  DC.SourceConfig ->
  SourceName ->
  HashMap DC.TableName (HashSet (ForeignKey 'DataConnector)) ->
  DC.TableName ->
  ArrRelDef 'DataConnector ->
  m (RelInfo 'DataConnector, Seq SchemaDependency)
buildArrayRelationshipInfo' sourceConfig sourceName fks tableName arrRel =
  ifSupportsLocalRelationships sourceName sourceConfig
    $ defaultBuildArrayRelationshipInfo sourceName fks tableName arrRel

ifSupportsLocalRelationships :: (MonadError QErr m) => SourceName -> DC.SourceConfig -> m a -> m a
ifSupportsLocalRelationships sourceName DC.SourceConfig {..} action = do
  let supportsRelationships = isJust $ API._cRelationships _scCapabilities
  let supportsRemoteRelationships = isJust $ API._qcForeach =<< API._cQueries _scCapabilities
  if supportsRelationships
    then action
    else
      let suggestion =
            if supportsRemoteRelationships
              then " Instead consider using remote relationships to join between tables on the same source."
              else ""
       in throw400 NotSupported $ "Local object and array relationships are not supported for '" <> toTxt sourceName <> "'." <> suggestion

supportsBeingRemoteRelationshipTarget' :: DC.SourceConfig -> Bool
supportsBeingRemoteRelationshipTarget' DC.SourceConfig {..} =
  isJust $ API._qcForeach =<< API._cQueries _scCapabilities

listAllTables' :: (CacheRM m, Has (Logger Hasura) r, MonadIO m, MonadBaseControl IO m, MonadReader r m, MonadError QErr m, MetadataM m) => SourceName -> m [DC.TableName]
listAllTables' sourceName = do
  (logger :: Logger Hasura) <- asks getter
  sourceConfig <- askSourceConfig @'DataConnector sourceName
  schemaResponse <-
    if supportsSchemaPost' sourceConfig
      then requestDatabaseSchemaPost logger sourceName sourceConfig (API.SchemaRequest mempty API.BasicInfo)
      else requestDatabaseSchemaGet logger sourceName sourceConfig
  pure $ fmap (Witch.from . API._tiName) $ API._srTables schemaResponse

listAllTrackables' :: (CacheRM m, Has (Logger Hasura) r, MonadIO m, MonadBaseControl IO m, MonadReader r m, MonadError QErr m, MetadataM m) => SourceName -> m (TrackableInfo 'DataConnector)
listAllTrackables' sourceName = do
  (logger :: Logger Hasura) <- asks getter
  sourceConfig <- askSourceConfig @'DataConnector sourceName
  schemaResponse <-
    if supportsSchemaPost' sourceConfig
      then requestDatabaseSchemaPost logger sourceName sourceConfig (API.SchemaRequest mempty API.BasicInfo)
      else requestDatabaseSchemaGet logger sourceName sourceConfig
  let functions = fmap (\fi -> TrackableFunctionInfo (Witch.into (API._fiName fi)) (getVolatility (API._fiFunctionType fi))) $ API._srFunctions schemaResponse
  let tables = fmap (TrackableTableInfo . Witch.into . API._tiName) $ API._srTables schemaResponse
  pure
    TrackableInfo
      { trackableTables = tables,
        trackableFunctions = functions
      }

supportsSchemaPost' :: DC.SourceConfig -> Bool
supportsSchemaPost' DC.SourceConfig {..} =
  isJust $ API._cPostSchema _scCapabilities

getVolatility :: API.FunctionType -> FunctionVolatility
getVolatility API.FRead = FTSTABLE
getVolatility API.FWrite = FTVOLATILE

getTableInfo' :: (CacheRM m, MetadataM m, MonadError QErr m) => SourceName -> DC.TableName -> m (Maybe (SourceTableInfo 'DataConnector))
getTableInfo' sourceName tableName = do
  SourceInfo {_siDbObjectsIntrospection, _siTables, _siLogicalModels} <- askSourceInfo @'DataConnector sourceName

  let tables :: HashMap DC.TableName (RQL.T.T.DBTableMetadata 'DataConnector)
      tables = _rsTables _siDbObjectsIntrospection

  pure $ convertTableMetadataToTableInfo tableName _siLogicalModels <$> HashMap.lookup tableName tables <*> HashMap.lookup tableName _siTables

convertTableMetadataToTableInfo :: DC.TableName -> LogicalModelCache 'DataConnector -> RQL.T.T.DBTableMetadata 'DataConnector -> RQL.T.T.TableInfo 'DataConnector -> SourceTableInfo 'DataConnector
convertTableMetadataToTableInfo tableName logicalModelCache RQL.T.T.DBTableMetadata {..} RQL.T.T.TableInfo {..} =
  SourceTableInfo
    { _stiName = Witch.from tableName,
      _stiType = case DC._etmTableType _ptmiExtraTableMetadata of
        DC.Table -> Table
        DC.View -> View,
      _stiColumns = convertColumn <$> RQL.T.T._tciRawColumns _tiCoreInfo,
      _stiLogicalModels = fmap logicalModelInfoToMetadata . HashMap.elems $ foldl' collectLogicalModels mempty $ RQL.T.C.rciType <$> RQL.T.T._tciRawColumns _tiCoreInfo,
      _stiPrimaryKey = fmap Witch.from . toNonEmpty . RQL.T.T._pkColumns <$> _ptmiPrimaryKey,
      _stiForeignKeys = convertForeignKeys _ptmiForeignKeys,
      _stiDescription = getPGDescription <$> _ptmiDescription,
      _stiInsertable = all RQL.T.T.viIsInsertable _ptmiViewInfo,
      _stiUpdatable = all RQL.T.T.viIsUpdatable _ptmiViewInfo,
      _stiDeletable = all RQL.T.T.viIsDeletable _ptmiViewInfo
    }
  where
    convertColumn :: RQL.T.C.RawColumnInfo 'DataConnector -> SourceColumnInfo 'DataConnector
    convertColumn RQL.T.C.RawColumnInfo {..} =
      SourceColumnInfo
        { _sciName = Witch.from rciName,
          _sciType = Witch.from rciType,
          _sciNullable = rciIsNullable,
          _sciDescription = G.unDescription <$> rciDescription,
          _sciInsertable = RQL.T.C._cmIsInsertable rciMutability,
          _sciUpdatable = RQL.T.C._cmIsUpdatable rciMutability,
          _sciValueGenerated =
            extraColumnMetadata
              >>= DC._ecmValueGenerated
              <&> \case
                API.AutoIncrement -> AutoIncrement
                API.UniqueIdentifier -> UniqueIdentifier
                API.DefaultValue -> DefaultValue
        }
      where
        extraColumnMetadata = HashMap.lookup rciName . DC._etmExtraColumnMetadata $ _ptmiExtraTableMetadata

    collectLogicalModels :: LogicalModelCache 'DataConnector -> RQL.T.C.RawColumnType 'DataConnector -> LogicalModelCache 'DataConnector
    collectLogicalModels seenLogicalModels = \case
      RQL.T.C.RawColumnTypeScalar _ -> seenLogicalModels
      RQL.T.C.RawColumnTypeObject _ name -> collectLogicalModelName seenLogicalModels (LogicalModelName name)
      RQL.T.C.RawColumnTypeArray _ rawColumnType _ -> collectLogicalModels seenLogicalModels rawColumnType

    collectLogicalModelName :: LogicalModelCache 'DataConnector -> LogicalModelName -> LogicalModelCache 'DataConnector
    collectLogicalModelName seenLogicalModels logicalModelName
      | logicalModelName `HashMap.member` seenLogicalModels = seenLogicalModels
      | otherwise =
          case HashMap.lookup logicalModelName logicalModelCache of
            Nothing -> seenLogicalModels
            Just logicalModelInfo ->
              let seenLogicalModels' = HashMap.insert logicalModelName logicalModelInfo seenLogicalModels
               in foldl' collectLogicalModelType seenLogicalModels' (fmap lmfType $ InsOrdHashMap.elems $ _lmiFields logicalModelInfo)

    collectLogicalModelType :: LogicalModelCache 'DataConnector -> LogicalModelType 'DataConnector -> LogicalModelCache 'DataConnector
    collectLogicalModelType seenLogicalModels = \case
      LogicalModelTypeScalar _ -> seenLogicalModels
      LogicalModelTypeArray LogicalModelTypeArrayC {..} -> collectLogicalModelType seenLogicalModels lmtaArray
      LogicalModelTypeReference LogicalModelTypeReferenceC {..} -> collectLogicalModelName seenLogicalModels lmtrReference

    logicalModelInfoToMetadata :: LogicalModelInfo 'DataConnector -> LogicalModelMetadata 'DataConnector
    logicalModelInfoToMetadata LogicalModelInfo {..} =
      LogicalModelMetadata
        { _lmmName = _lmiName,
          _lmmFields = _lmiFields,
          _lmmDescription = _lmiDescription,
          _lmmSelectPermissions = mempty
        }

    convertForeignKeys :: HashSet (RQL.T.T.ForeignKeyMetadata 'DataConnector) -> SourceForeignKeys 'DataConnector
    convertForeignKeys foreignKeys =
      foreignKeys
        & HashSet.toList
        & fmap
          ( \(RQL.T.T.ForeignKeyMetadata RQL.T.T.ForeignKey {..}) ->
              let constraintName = RQL.T.T._cName _fkConstraint
                  constraint =
                    SourceConstraint
                      { _scForeignTable = Witch.from _fkForeignTable,
                        _scColumnMapping = RelMapping $ NEHashMap.toHashMap _fkColumnMapping
                      }
               in (constraintName, constraint)
          )
        & HashMap.fromList
        & SourceForeignKeys
