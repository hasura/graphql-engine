{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataConnector.Adapter.Metadata (requestDatabaseSchema) where

import Control.Arrow.Extended
import Control.Monad.Trans.Control
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Bifunctor (bimap)
import Data.Environment (Environment)
import Data.Has (Has (getter))
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashMap.Strict.NonEmpty qualified as NEHashMap
import Data.HashSet qualified as HashSet
import Data.Map.Strict qualified as Map
import Data.Semigroup.Foldable (Foldable1 (..))
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text.Extended (toTxt, (<<>), (<>>))
import Hasura.Backends.DataConnector.API (capabilitiesCase, errorResponseSummary, schemaCase)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.API.V0.ErrorResponse (_crDetails)
import Hasura.Backends.DataConnector.API.V0.Table qualified as DC (TableType (..))
import Hasura.Backends.DataConnector.Adapter.Backend (columnTypeToScalarType)
import Hasura.Backends.DataConnector.Adapter.ConfigTransform (transformSourceConfig, validateConnSourceConfig)
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC
import Hasura.Backends.DataConnector.Agent.Client (AgentClientContext (..), runAgentClientT)
import Hasura.Backends.Postgres.SQL.Types (PGDescription (..))
import Hasura.Base.Error (Code (..), QErr (..), decodeValue, throw400, throw400WithDetail, withPathK)
import Hasura.Incremental qualified as Inc
import Hasura.Incremental.Select qualified as Inc
import Hasura.Logging (Hasura, Logger)
import Hasura.Prelude
import Hasura.RQL.DDL.Relationship (defaultBuildArrayRelationshipInfo, defaultBuildObjectRelationshipInfo)
import Hasura.RQL.IR.BoolExp (OpExpG (..), PartialSQLExp (..), RootOrCurrent (..), RootOrCurrentColumn (..))
import Hasura.RQL.Types.BackendType (BackendSourceKind (..), BackendType (..))
import Hasura.RQL.Types.Column qualified as RQL.T.C
import Hasura.RQL.Types.Common (OID (..), SourceName)
import Hasura.RQL.Types.CustomTypes (GraphQLType (..))
import Hasura.RQL.Types.EventTrigger (RecreateEventTriggers (RETDoNothing))
import Hasura.RQL.Types.Metadata (SourceMetadata (..))
import Hasura.RQL.Types.Metadata.Backend (BackendMetadata (..))
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Relationships.Local (ArrRelDef, ObjRelDef, RelInfo)
import Hasura.RQL.Types.SchemaCache (CacheRM, askSourceConfig, askSourceInfo)
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.SchemaCacheTypes (SchemaDependency)
import Hasura.RQL.Types.Source (DBObjectsIntrospection (..), SourceInfo (..))
import Hasura.RQL.Types.Source.Column (ColumnValueGenerationStrategy (..), SourceColumnInfo (..))
import Hasura.RQL.Types.Source.Table (SourceConstraint (..), SourceForeignKeys (..), SourceTableInfo (..), SourceTableType (..))
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
import Servant.Client ((//))
import Servant.Client.Generic (genericClient)
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

  -- If/when we implement enums for Data Connector backend, we will also need to fix columnTypeToScalarType function
  -- in Hasura.Backends.DataConnector.Adapter.Backend. See note there for more information.
  fetchAndValidateEnumValues = error "fetchAndValidateEnumValues: not implemented for the Data Connector backend."

  buildArrayRelationshipInfo = buildArrayRelationshipInfo'
  buildObjectRelationshipInfo = buildObjectRelationshipInfo'

  buildFunctionInfo = error "buildFunctionInfo: not implemented for the Data Connector backend."
  updateColumnInEventTrigger = error "updateColumnInEventTrigger: not implemented for the Data Connector backend."
  postDropSourceHook _sourceConfig _tableTriggerMap = pure ()
  buildComputedFieldBooleanExp _ _ _ _ _ _ =
    error "buildComputedFieldBooleanExp: not implemented for the Data Connector backend."
  columnInfoToFieldInfo = columnInfoToFieldInfo'
  listAllTables = listAllTables'
  getTableInfo = getTableInfo'
  supportsBeingRemoteRelationshipTarget = supportsBeingRemoteRelationshipTarget'

resolveBackendInfo' ::
  forall arr m.
  ( ArrowChoice arr,
    Inc.ArrowCache m arr,
    Inc.ArrowDistribute arr,
    ArrowWriter (Seq (Either InconsistentMetadata MetadataDependency)) arr,
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
      |) (toHashMap optionsMap)
  returnA -< HashMap.catMaybes maybeDataConnectorCapabilities
  where
    getDataConnectorCapabilitiesIfNeeded ::
      (Inc.Dependency (Maybe (HashMap DC.DataConnectorName Inc.InvalidationKey)), DC.DataConnectorName, DC.DataConnectorOptions) `arr` Maybe DC.DataConnectorInfo
    getDataConnectorCapabilitiesIfNeeded = Inc.cache proc (invalidationKeys, dataConnectorName, dataConnectorOptions) -> do
      let metadataObj = MetadataObject (MODataConnectorAgent dataConnectorName) $ J.toJSON dataConnectorName
      httpMgr <- bindA -< askHTTPManager
      Inc.dependOn -< Inc.selectMaybeD (Inc.ConstS dataConnectorName) invalidationKeys
      (|
        withRecordInconsistency
          ( bindErrorA -< ExceptT $ getDataConnectorCapabilities dataConnectorOptions httpMgr
          )
        |) metadataObj

    getDataConnectorCapabilities ::
      DC.DataConnectorOptions ->
      HTTP.Manager ->
      m (Either QErr DC.DataConnectorInfo)
    getDataConnectorCapabilities options@DC.DataConnectorOptions {..} manager = runExceptT do
      capabilitiesU <-
        ignoreTraceT
          . flip runAgentClientT (AgentClientContext logger _dcoUri manager Nothing Nothing)
          $ genericClient @API.Routes // API._capabilities

      let defaultAction = throw400 DataConnectorError "Unexpected data connector capabilities response - Unexpected Type"
          capabilitiesAction API.CapabilitiesResponse {..} = pure $ DC.DataConnectorInfo options _crCapabilities _crConfigSchemaResponse _crDisplayName _crReleaseName

      capabilitiesCase defaultAction capabilitiesAction errorAction capabilitiesU

    toHashMap = HashMap.fromList . Map.toList

resolveSourceConfig' ::
  (Monad m) =>
  SourceName ->
  DC.ConnSourceConfig ->
  BackendSourceKind 'DataConnector ->
  HashMap DC.DataConnectorName DC.DataConnectorInfo ->
  Environment ->
  HTTP.Manager ->
  m (Either QErr DC.SourceConfig)
resolveSourceConfig'
  sourceName
  csc@DC.ConnSourceConfig {template, timeout, value = originalConfig}
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
          _scTemplate = template,
          _scCapabilities = _dciCapabilities,
          _scManager = manager,
          _scTimeoutMicroseconds = (DC.sourceTimeoutMicroseconds <$> timeout),
          _scDataConnectorName = dataConnectorName,
          _scEnvironment = env
        }

getDataConnectorInfo :: (MonadError QErr m) => DC.DataConnectorName -> HashMap DC.DataConnectorName DC.DataConnectorInfo -> m DC.DataConnectorInfo
getDataConnectorInfo dataConnectorName backendInfo =
  onNothing (HashMap.lookup dataConnectorName backendInfo) $
    throw400 DataConnectorError ("Data connector named " <> toTxt dataConnectorName <<> " was not found in the data connector backend info")

resolveDatabaseMetadata' ::
  ( MonadIO m,
    MonadBaseControl IO m
  ) =>
  Logger Hasura ->
  SourceMetadata 'DataConnector ->
  DC.SourceConfig ->
  m (Either QErr (DBObjectsIntrospection 'DataConnector))
resolveDatabaseMetadata' logger SourceMetadata {_smName} sourceConfig@DC.SourceConfig {_scCapabilities} = runExceptT do
  API.SchemaResponse {..} <- requestDatabaseSchema logger _smName sourceConfig
  let typeNames = maybe mempty (HashSet.fromList . toList . fmap API._otdName) _srObjectTypes
      customObjectTypes =
        maybe mempty (HashMap.fromList . mapMaybe (toTableObjectType _scCapabilities typeNames) . toList) _srObjectTypes
      tables = HashMap.fromList $ do
        API.TableInfo {..} <- _srTables
        let primaryKeyColumns = fmap Witch.from . NESeq.fromList <$> _tiPrimaryKey
        let meta =
              RQL.T.T.DBTableMetadata
                { _ptmiOid = OID 0, -- TODO: This is wrong and needs to be fixed. It is used for diffing tables and seeing what's new/deleted/altered, so reusing 0 for all tables is problematic.
                  _ptmiColumns = do
                    API.ColumnInfo {..} <- _tiColumns
                    pure $
                      RQL.T.C.RawColumnInfo
                        { rciName = Witch.from _ciName,
                          rciPosition = 1, -- TODO: This is very wrong and needs to be fixed. It is used for diffing tables and seeing what's new/deleted/altered, so reusing 1 for all columns is problematic.
                          rciType = DC.mkScalarType _scCapabilities _ciType,
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
                      },
                  _ptmiCustomObjectTypes = Just customObjectTypes
                }
        pure (coerce _tiName, meta)
   in pure $
        DBObjectsIntrospection
          { _rsTables = tables,
            _rsFunctions = mempty,
            _rsScalars = mempty
          }

requestDatabaseSchema ::
  (MonadIO m, MonadBaseControl IO m, MonadError QErr m) =>
  Logger Hasura ->
  SourceName ->
  DC.SourceConfig ->
  m API.SchemaResponse
requestDatabaseSchema logger sourceName sourceConfig = do
  transformedSourceConfig <- transformSourceConfig sourceConfig Nothing

  schemaResponseU <-
    ignoreTraceT
      . flip runAgentClientT (AgentClientContext logger (DC._scEndpoint transformedSourceConfig) (DC._scManager transformedSourceConfig) (DC._scTimeoutMicroseconds transformedSourceConfig) Nothing)
      $ (genericClient // API._schema) (toTxt sourceName) (DC._scConfig transformedSourceConfig)

  let defaultAction = throw400 DataConnectorError "Unexpected data connector schema response - Unexpected Type"

  schemaCase defaultAction pure errorAction schemaResponseU

toTableObjectType :: API.Capabilities -> HashSet G.Name -> API.ObjectTypeDefinition -> Maybe (G.Name, RQL.T.T.TableObjectType 'DataConnector)
toTableObjectType capabilities typeNames API.ObjectTypeDefinition {..} =
  (_otdName,) . RQL.T.T.TableObjectType _otdName (G.Description <$> _otdDescription) <$> traverse toTableObjectFieldDefinition _otdColumns
  where
    toTableObjectFieldDefinition API.ColumnInfo {..} = do
      fieldTypeName <- G.mkName $ API.getScalarType _ciType
      fieldName <- G.mkName $ API.unColumnName _ciName
      pure $
        RQL.T.T.TableObjectFieldDefinition
          { _tofdColumn = Witch.from _ciName,
            _tofdName = fieldName,
            _tofdDescription = G.Description <$> _ciDescription,
            _tofdGType = GraphQLType $ G.TypeNamed (G.Nullability _ciNullable) fieldTypeName,
            _tofdFieldType =
              if HashSet.member fieldTypeName typeNames
                then RQL.T.T.TOFTObject fieldTypeName
                else RQL.T.T.TOFTScalar fieldTypeName $ DC.mkScalarType capabilities _ciType
          }

-- | Construct a 'HashSet' 'RQL.T.T.ForeignKeyMetadata'
-- 'DataConnector' to build the foreign key constraints in the table
-- metadata.
buildForeignKeySet :: API.ForeignKeys -> HashSet (RQL.T.T.ForeignKeyMetadata 'DataConnector)
buildForeignKeySet (API.ForeignKeys constraints) =
  HashSet.fromList $
    constraints & HashMap.foldMapWithKey @[RQL.T.T.ForeignKeyMetadata 'DataConnector]
      \constraintName API.Constraint {..} -> maybeToList do
        let columnMapAssocList = HashMap.foldrWithKey' (\(API.ColumnName k) (API.ColumnName v) acc -> (DC.ColumnName k, DC.ColumnName v) : acc) [] _cColumnMapping
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
      v -> pure . AEQ False <$> parseWithTy columnType v

    parseOperation :: (Text, J.Value) -> m (OpExpG 'DataConnector v)
    parseOperation (opStr, val) = withPathK opStr $
      case opStr of
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

        parseEq = AEQ False <$> parseOne
        parseNeq = ANE False <$> parseOne
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
          when (columnType /= rhsType) $
            throw400 UnexpectedPayload $
              "incompatible column types: "
                <> columnRef <<> " has type "
                <> columnType <<> ", but "
                <> rhsCol <<> " has type " <>> rhsType
          pure rhsCol

parseCollectableType' ::
  (MonadError QErr m) =>
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
    CollectableTypeArray _ ->
      throw400 NotSupported "Array types are not supported by the Data Connector backend"

mkTypedSessionVar ::
  CollectableType (RQL.T.C.ColumnType 'DataConnector) ->
  SessionVariable ->
  PartialSQLExp 'DataConnector
mkTypedSessionVar columnType =
  PSESessVar (columnTypeToScalarType <$> columnType)

errorAction :: (MonadError QErr m) => API.ErrorResponse -> m a
errorAction e = throw400WithDetail DataConnectorError (errorResponseSummary e) (_crDetails e)

-- | This function assumes that if a type name is present in the custom object types for the table then it
-- refers to a nested object of that type.
-- Otherwise it is a normal (scalar) column.
columnInfoToFieldInfo' :: HashMap G.Name (RQL.T.T.TableObjectType 'DataConnector) -> RQL.T.C.ColumnInfo 'DataConnector -> RQL.T.T.FieldInfo 'DataConnector
columnInfoToFieldInfo' gqlTypes columnInfo@RQL.T.C.ColumnInfo {..} =
  maybe (RQL.T.T.FIColumn columnInfo) RQL.T.T.FINestedObject getNestedObjectInfo
  where
    getNestedObjectInfo =
      case ciType of
        RQL.T.C.ColumnScalar (DC.ScalarType scalarTypeName _) -> do
          gqlName <- GQL.mkName scalarTypeName
          guard $ HashMap.member gqlName gqlTypes
          pure $
            RQL.T.C.NestedObjectInfo
              { RQL.T.C._noiSupportsNestedObjects = (),
                RQL.T.C._noiColumn = ciColumn,
                RQL.T.C._noiName = ciName,
                RQL.T.C._noiType = gqlName,
                RQL.T.C._noiIsNullable = ciIsNullable,
                RQL.T.C._noiDescription = ciDescription,
                RQL.T.C._noiMutability = ciMutability
              }
        RQL.T.C.ColumnEnumReference {} -> Nothing

buildObjectRelationshipInfo' ::
  (MonadError QErr m) =>
  DC.SourceConfig ->
  SourceName ->
  HashMap DC.TableName (HashSet (ForeignKey 'DataConnector)) ->
  DC.TableName ->
  ObjRelDef 'DataConnector ->
  m (RelInfo 'DataConnector, Seq SchemaDependency)
buildObjectRelationshipInfo' sourceConfig sourceName fks tableName objRel = do
  ifSupportsLocalRelationships sourceName sourceConfig $
    defaultBuildObjectRelationshipInfo sourceName fks tableName objRel

buildArrayRelationshipInfo' ::
  (MonadError QErr m) =>
  DC.SourceConfig ->
  SourceName ->
  HashMap DC.TableName (HashSet (ForeignKey 'DataConnector)) ->
  DC.TableName ->
  ArrRelDef 'DataConnector ->
  m (RelInfo 'DataConnector, Seq SchemaDependency)
buildArrayRelationshipInfo' sourceConfig sourceName fks tableName arrRel =
  ifSupportsLocalRelationships sourceName sourceConfig $
    defaultBuildArrayRelationshipInfo sourceName fks tableName arrRel

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
  schemaResponse <- requestDatabaseSchema logger sourceName sourceConfig
  pure $ fmap (Witch.from . API._tiName) $ API._srTables schemaResponse

getTableInfo' :: (CacheRM m, MetadataM m, MonadError QErr m) => SourceName -> DC.TableName -> m (Maybe (SourceTableInfo 'DataConnector))
getTableInfo' sourceName tableName = do
  SourceInfo {_siDbObjectsIntrospection} <- askSourceInfo @'DataConnector sourceName

  let tables :: HashMap DC.TableName (RQL.T.T.DBTableMetadata 'DataConnector)
      tables = _rsTables _siDbObjectsIntrospection

  pure $ fmap (convertTableMetadataToTableInfo tableName) (HashMap.lookup tableName tables)

convertTableMetadataToTableInfo :: DC.TableName -> RQL.T.T.DBTableMetadata 'DataConnector -> SourceTableInfo 'DataConnector
convertTableMetadataToTableInfo tableName RQL.T.T.DBTableMetadata {..} =
  SourceTableInfo
    { _stiName = Witch.from tableName,
      _stiType = case DC._etmTableType _ptmiExtraTableMetadata of
        DC.Table -> Table
        DC.View -> View,
      _stiColumns = convertColumn <$> _ptmiColumns,
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
              >>= pure . \case
                API.AutoIncrement -> AutoIncrement
                API.UniqueIdentifier -> UniqueIdentifier
                API.DefaultValue -> DefaultValue
        }
      where
        extraColumnMetadata = HashMap.lookup rciName . DC._etmExtraColumnMetadata $ _ptmiExtraTableMetadata

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
                        _scColumnMapping = HashMap.fromList $ bimap Witch.from Witch.from <$> NEHashMap.toList _fkColumnMapping
                      }
               in (constraintName, constraint)
          )
        & HashMap.fromList
        & SourceForeignKeys
