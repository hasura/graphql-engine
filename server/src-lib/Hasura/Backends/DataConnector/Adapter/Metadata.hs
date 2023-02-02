{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataConnector.Adapter.Metadata () where

import Control.Arrow.Extended
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Environment (Environment)
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.HashMap.Strict.NonEmpty qualified as NEHashMap
import Data.HashSet qualified as HashSet
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text.Extended (toTxt, (<<>), (<>>))
import Hasura.Backends.DataConnector.API (capabilitiesCase, errorResponseSummary, schemaCase)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.API.V0.ErrorResponse (_crDetails)
import Hasura.Backends.DataConnector.Adapter.Backend (columnTypeToScalarType)
import Hasura.Backends.DataConnector.Adapter.ConfigTransform (transformConnSourceConfig)
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC
import Hasura.Backends.DataConnector.Agent.Client (AgentClientContext (..), runAgentClientT)
import Hasura.Backends.Postgres.SQL.Types (PGDescription (..))
import Hasura.Base.Error (Code (..), QErr (..), decodeValue, throw400, throw400WithDetail, throw500, withPathK)
import Hasura.Incremental qualified as Inc
import Hasura.Incremental.Select qualified as Inc
import Hasura.Logging (Hasura, Logger)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (OpExpG (..), PartialSQLExp (..), RootOrCurrent (..), RootOrCurrentColumn (..))
import Hasura.RQL.Types.Column qualified as RQL.T.C
import Hasura.RQL.Types.Common (OID (..), SourceName)
import Hasura.RQL.Types.EventTrigger (RecreateEventTriggers (RETDoNothing))
import Hasura.RQL.Types.Metadata (SourceMetadata (..))
import Hasura.RQL.Types.Metadata.Backend (BackendMetadata (..))
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.SchemaCache qualified as SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.Source (DBObjectsIntrospection (..))
import Hasura.RQL.Types.Table (ForeignKey (_fkConstraint))
import Hasura.RQL.Types.Table qualified as RQL.T.T
import Hasura.SQL.Backend (BackendSourceKind (..), BackendType (..))
import Hasura.SQL.Types (CollectableType (..))
import Hasura.Server.Migrate.Version (SourceCatalogMigrationState (..))
import Hasura.Server.Utils qualified as HSU
import Hasura.Session (SessionVariable, mkSessionVariable)
import Hasura.Tracing (ignoreTraceT)
import Language.GraphQL.Draft.Syntax qualified as GQL
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.Manager
import Servant.Client.Core.HasClient ((//))
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

  buildFunctionInfo = error "buildFunctionInfo: not implemented for the Data Connector backend."
  updateColumnInEventTrigger = error "updateColumnInEventTrigger: not implemented for the Data Connector backend."
  postDropSourceHook _sourceConfig _tableTriggerMap = pure ()
  buildComputedFieldBooleanExp _ _ _ _ _ _ =
    error "buildComputedFieldBooleanExp: not implemented for the Data Connector backend."

resolveBackendInfo' ::
  ( ArrowChoice arr,
    Inc.ArrowCache m arr,
    Inc.ArrowDistribute arr,
    ArrowWriter (Seq (Either InconsistentMetadata MetadataDependency)) arr,
    MonadIO m,
    MonadBaseControl IO m,
    HasHttpManagerM m
  ) =>
  Logger Hasura ->
  (Inc.Dependency (Maybe (HashMap DC.DataConnectorName Inc.InvalidationKey)), InsOrdHashMap DC.DataConnectorName DC.DataConnectorOptions) `arr` HashMap DC.DataConnectorName DC.DataConnectorInfo
resolveBackendInfo' logger = proc (invalidationKeys, optionsMap) -> do
  maybeDataConnectorCapabilities <-
    (|
      Inc.keyed
        ( \dataConnectorName dataConnectorOptions -> do
            getDataConnectorCapabilitiesIfNeeded -< (invalidationKeys, dataConnectorName, dataConnectorOptions)
        )
      |) (OMap.toHashMap optionsMap)
  returnA -< HashMap.catMaybes maybeDataConnectorCapabilities
  where
    getDataConnectorCapabilitiesIfNeeded ::
      forall arr m.
      ( ArrowChoice arr,
        Inc.ArrowCache m arr,
        ArrowWriter (Seq (Either InconsistentMetadata MetadataDependency)) arr,
        MonadIO m,
        MonadBaseControl IO m,
        HasHttpManagerM m
      ) =>
      (Inc.Dependency (Maybe (HashMap DC.DataConnectorName Inc.InvalidationKey)), DC.DataConnectorName, DC.DataConnectorOptions) `arr` Maybe DC.DataConnectorInfo
    getDataConnectorCapabilitiesIfNeeded = Inc.cache proc (invalidationKeys, dataConnectorName, dataConnectorOptions) -> do
      let metadataObj = MetadataObject (MODataConnectorAgent dataConnectorName) $ J.toJSON dataConnectorName
      httpMgr <- bindA -< askHttpManager
      Inc.dependOn -< Inc.selectMaybeD (Inc.ConstS dataConnectorName) invalidationKeys
      (|
        withRecordInconsistency
          ( liftEitherA <<< bindA -< getDataConnectorCapabilities dataConnectorOptions httpMgr
          )
        |) metadataObj

    getDataConnectorCapabilities ::
      (MonadIO m, MonadBaseControl IO m) =>
      DC.DataConnectorOptions ->
      HTTP.Manager ->
      m (Either QErr DC.DataConnectorInfo)
    getDataConnectorCapabilities options@DC.DataConnectorOptions {..} manager = runExceptT do
      capabilitiesU <-
        ignoreTraceT
          . flip runAgentClientT (AgentClientContext logger _dcoUri manager Nothing)
          $ genericClient // API._capabilities

      let defaultAction = throw400 DataConnectorError "Unexpected data connector capabilities response - Unexpected Type"
          capabilitiesAction API.CapabilitiesResponse {..} = pure $ DC.DataConnectorInfo options _crCapabilities _crConfigSchemaResponse _crDisplayName _crReleaseName

      capabilitiesCase defaultAction capabilitiesAction errorAction capabilitiesU

resolveSourceConfig' ::
  (MonadIO m, MonadBaseControl IO m) =>
  Logger Hasura ->
  SourceName ->
  DC.ConnSourceConfig ->
  BackendSourceKind 'DataConnector ->
  HashMap DC.DataConnectorName DC.DataConnectorInfo ->
  Environment ->
  HTTP.Manager ->
  m (Either QErr DC.SourceConfig)
resolveSourceConfig'
  logger
  sourceName
  csc@DC.ConnSourceConfig {template, timeout, value = originalConfig}
  (DataConnectorKind dataConnectorName)
  backendInfo
  env
  manager = runExceptT do
    DC.DataConnectorInfo {..} <- getDataConnectorInfo dataConnectorName backendInfo
    let DC.DataConnectorOptions {_dcoUri} = _dciOptions

    transformedConfig <- transformConnSourceConfig dataConnectorName sourceName _dciConfigSchemaResponse csc [("$session", J.object []), ("$env", J.toJSON env)] env

    schemaResponseU <-
      ignoreTraceT
        . flip runAgentClientT (AgentClientContext logger _dcoUri manager (DC.sourceTimeoutMicroseconds <$> timeout))
        $ (genericClient // API._schema) (toTxt sourceName) transformedConfig

    let defaultAction = throw400 DataConnectorError "Unexpected data connector schema response - Unexpected Type"

    schemaResponse <- schemaCase defaultAction pure errorAction schemaResponseU

    pure
      DC.SourceConfig
        { _scEndpoint = _dcoUri,
          _scConfig = originalConfig,
          _scTemplate = template,
          _scCapabilities = _dciCapabilities,
          _scSchema = schemaResponse,
          _scManager = manager,
          _scTimeoutMicroseconds = (DC.sourceTimeoutMicroseconds <$> timeout),
          _scDataConnectorName = dataConnectorName
        }

getDataConnectorInfo :: (MonadError QErr m) => DC.DataConnectorName -> HashMap DC.DataConnectorName DC.DataConnectorInfo -> m DC.DataConnectorInfo
getDataConnectorInfo dataConnectorName backendInfo =
  onNothing (Map.lookup dataConnectorName backendInfo) $
    throw400 DataConnectorError ("Data connector named " <> toTxt dataConnectorName <<> " was not found in the data connector backend info")

resolveDatabaseMetadata' ::
  Applicative m =>
  SourceMetadata 'DataConnector ->
  DC.SourceConfig ->
  m (Either QErr (DBObjectsIntrospection 'DataConnector))
resolveDatabaseMetadata' _ DC.SourceConfig {_scSchema = API.SchemaResponse {..}, ..} =
  let tables = Map.fromList $ do
        API.TableInfo {..} <- _srTables
        let primaryKeyColumns = Seq.fromList $ coerce <$> _tiPrimaryKey
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
                  _ptmiPrimaryKey = RQL.T.T.PrimaryKey (RQL.T.T.Constraint (DC.ConstraintName "") (OID 0)) <$> NESeq.nonEmptySeq primaryKeyColumns,
                  _ptmiUniqueConstraints = mempty,
                  _ptmiForeignKeys = buildForeignKeySet _tiForeignKeys,
                  _ptmiViewInfo =
                    ( if _tiType == API.Table && _tiInsertable && _tiUpdatable && _tiDeletable
                        then Nothing
                        else Just $ RQL.T.T.ViewInfo _tiInsertable _tiUpdatable _tiDeletable
                    ),
                  _ptmiDescription = fmap PGDescription _tiDescription,
                  _ptmiExtraTableMetadata = ()
                }
        pure (coerce _tiName, meta)
   in pure $
        pure $
          DBObjectsIntrospection
            { _rsTables = tables,
              _rsFunctions = mempty,
              _rsScalars = mempty
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
  (MonadError QErr m, SchemaCache.TableCoreInfoRM 'DataConnector m) =>
  RQL.T.C.ValueParser 'DataConnector m v ->
  DC.TableName ->
  RQL.T.T.FieldInfoMap (RQL.T.T.FieldInfo 'DataConnector) ->
  RQL.T.C.ColumnReference 'DataConnector ->
  J.Value ->
  m [OpExpG 'DataConnector v]
parseBoolExpOperations' rhsParser rootTable fieldInfoMap columnRef value =
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
            [J.String "$", col] -> do
              rootTableInfo <-
                SchemaCache.lookupTableCoreInfo rootTable
                  >>= flip onNothing (throw500 $ "unexpected: " <> rootTable <<> " doesn't exist")
              go IsRoot (RQL.T.T._tciFieldInfoMap rootTableInfo) col
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
  MonadError QErr m =>
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

errorAction :: MonadError QErr m => API.ErrorResponse -> m a
errorAction e = throw400WithDetail DataConnectorError (errorResponseSummary e) (_crDetails e)
