{-# LANGUAGE Arrows #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataConnector.Adapter.Metadata () where

import Control.Arrow.Extended
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
import Data.Text qualified as Text
import Data.Text.Extended (toTxt, (<<>), (<>>))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.ConfigTransform (transformConnSourceConfig)
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC
import Hasura.Backends.DataConnector.Agent.Client (AgentClientContext (..), runAgentClientT)
import Hasura.Backends.Postgres.SQL.Types (PGDescription (..))
import Hasura.Base.Error (Code (..), QErr, decodeValue, throw400, throw500, withPathK)
import Hasura.Incremental qualified as Inc
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
import Hasura.RQL.Types.Source (ResolvedSource (..))
import Hasura.RQL.Types.SourceCustomization (SourceTypeCustomization)
import Hasura.RQL.Types.Table (ForeignKey (_fkConstraint))
import Hasura.RQL.Types.Table qualified as RQL.T.T
import Hasura.SQL.Backend (BackendSourceKind (..), BackendType (..))
import Hasura.SQL.Types (CollectableType (..))
import Hasura.Server.Utils qualified as HSU
import Hasura.Session (SessionVariable, mkSessionVariable)
import Hasura.Tracing (noReporter, runTraceTWithReporter)
import Language.GraphQL.Draft.Syntax qualified as GQL
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.Manager
import Servant.Client.Core.HasClient ((//))
import Servant.Client.Generic (genericClient)
import Witch qualified

instance BackendMetadata 'DataConnector where
  prepareCatalog _ = pure RETDoNothing
  type BackendInvalidationKeys 'DataConnector = HashMap DC.DataConnectorName Inc.InvalidationKey
  resolveBackendInfo = resolveBackendInfo'
  resolveSourceConfig = resolveSourceConfig'
  resolveDatabaseMetadata = resolveDatabaseMetadata'
  parseBoolExpOperations = parseBoolExpOperations'
  parseCollectableType = parseCollectableType'
  buildComputedFieldInfo = error "buildComputedFieldInfo: not implemented for the Data Connector backend."
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
    ArrowWriter (Seq CollectedInfo) arr,
    MonadIO m,
    HasHttpManagerM m
  ) =>
  Logger Hasura ->
  (Inc.Dependency (HashMap DC.DataConnectorName Inc.InvalidationKey), InsOrdHashMap DC.DataConnectorName DC.DataConnectorOptions) `arr` HashMap DC.DataConnectorName DC.DataConnectorInfo
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
        ArrowWriter (Seq CollectedInfo) arr,
        MonadIO m,
        HasHttpManagerM m
      ) =>
      (Inc.Dependency (HashMap DC.DataConnectorName Inc.InvalidationKey), DC.DataConnectorName, DC.DataConnectorOptions) `arr` Maybe DC.DataConnectorInfo
    getDataConnectorCapabilitiesIfNeeded = Inc.cache proc (invalidationKeys, dataConnectorName, dataConnectorOptions) -> do
      let metadataObj = MetadataObject (MODataConnectorAgent dataConnectorName) $ J.toJSON dataConnectorName
      httpMgr <- bindA -< askHttpManager
      Inc.dependOn -< Inc.selectKeyD dataConnectorName invalidationKeys
      (|
        withRecordInconsistency
          ( liftEitherA <<< bindA -< getDataConnectorCapabilities dataConnectorOptions httpMgr
          )
        |) metadataObj

    getDataConnectorCapabilities ::
      MonadIO m =>
      DC.DataConnectorOptions ->
      HTTP.Manager ->
      m (Either QErr DC.DataConnectorInfo)
    getDataConnectorCapabilities options@DC.DataConnectorOptions {..} manager = runExceptT do
      API.CapabilitiesResponse {..} <-
        runTraceTWithReporter noReporter "capabilities"
          . flip runAgentClientT (AgentClientContext logger _dcoUri manager Nothing)
          $ genericClient // API._capabilities
      return $ DC.DataConnectorInfo options _crCapabilities _crConfigSchemaResponse

resolveSourceConfig' ::
  MonadIO m =>
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
    DC.DataConnectorInfo {DC._dciOptions = DC.DataConnectorOptions {..}, ..} <-
      Map.lookup dataConnectorName backendInfo
        `onNothing` throw400 DataConnectorError ("Data connector named " <> toTxt dataConnectorName <<> " was not found in the data connector backend info")

    transformedConfig <- transformConnSourceConfig csc [("$session", J.object []), ("$env", J.toJSON env)] env

    validateConfiguration sourceName dataConnectorName _dciConfigSchemaResponse transformedConfig

    schemaResponse <-
      runTraceTWithReporter noReporter "resolve source"
        . flip runAgentClientT (AgentClientContext logger _dcoUri manager (DC.sourceTimeoutMicroseconds <$> timeout))
        $ (genericClient // API._schema) (toTxt sourceName) transformedConfig

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

validateConfiguration ::
  MonadError QErr m =>
  SourceName ->
  DC.DataConnectorName ->
  API.ConfigSchemaResponse ->
  API.Config ->
  m ()
validateConfiguration sourceName dataConnectorName configSchema config = do
  let errors = API.validateConfigAgainstConfigSchema configSchema config
  unless (null errors) $
    let errorsText = Text.unlines (("- " <>) . Text.pack <$> errors)
     in throw400
          DataConnectorError
          ("Configuration for source " <> sourceName <<> " is not valid based on the configuration schema declared by the " <> dataConnectorName <<> " data connector agent. Errors:\n" <> errorsText)

resolveDatabaseMetadata' ::
  Applicative m =>
  SourceMetadata 'DataConnector ->
  DC.SourceConfig ->
  SourceTypeCustomization ->
  m (Either QErr (ResolvedSource 'DataConnector))
resolveDatabaseMetadata' _ sc@(DC.SourceConfig {_scSchema = API.SchemaResponse {..}}) customization =
  -- We need agents to provide the foreign key contraints inside 'API.SchemaResponse'
  let foreignKeys = fmap API._tiForeignKeys _srTables
      tables = Map.fromList $ do
        API.TableInfo {..} <- _srTables
        let primaryKeyColumns = Seq.fromList $ coerce <$> fromMaybe [] _tiPrimaryKey
        let meta =
              RQL.T.T.DBTableMetadata
                { _ptmiOid = OID 0,
                  _ptmiColumns = do
                    API.ColumnInfo {..} <- _tiColumns
                    pure $
                      RQL.T.C.RawColumnInfo
                        { rciName = Witch.from _ciName,
                          rciPosition = 1,
                          rciType = Witch.from _ciType,
                          rciIsNullable = _ciNullable,
                          rciDescription = fmap GQL.Description _ciDescription,
                          -- TODO: Add Column Mutability to the 'TableInfo'
                          rciMutability = RQL.T.C.ColumnMutability False False
                        },
                  _ptmiPrimaryKey = RQL.T.T.PrimaryKey (RQL.T.T.Constraint (DC.ConstraintName "") (OID 0)) <$> NESeq.nonEmptySeq primaryKeyColumns,
                  _ptmiUniqueConstraints = mempty,
                  _ptmiForeignKeys = buildForeignKeySet foreignKeys,
                  _ptmiViewInfo = Just $ RQL.T.T.ViewInfo False False False,
                  _ptmiDescription = fmap PGDescription _tiDescription,
                  _ptmiExtraTableMetadata = ()
                }
        pure (coerce _tiName, meta)
   in pure $
        pure $
          ResolvedSource
            { _rsConfig = sc,
              _rsCustomization = customization,
              _rsTables = tables,
              _rsFunctions = mempty,
              _rsScalars = mempty
            }

-- | Construct a 'HashSet' 'RQL.T.T.ForeignKeyMetadata'
-- 'DataConnector' to build the foreign key constraints in the table
-- metadata.
buildForeignKeySet :: [Maybe API.ForeignKeys] -> HashSet (RQL.T.T.ForeignKeyMetadata 'DataConnector)
buildForeignKeySet (catMaybes -> foreignKeys) =
  HashSet.fromList $
    join $
      foreignKeys <&> \(API.ForeignKeys constraints) ->
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
    CollectableTypeScalar scalarType ->
      PSESQLExp . DC.ValueLiteral <$> RQL.T.C.parseScalarValueColumnType scalarType val
    CollectableTypeArray _ ->
      throw400 NotSupported "Array types are not supported by the Data Connector backend"

mkTypedSessionVar ::
  CollectableType (RQL.T.C.ColumnType 'DataConnector) ->
  SessionVariable ->
  PartialSQLExp 'DataConnector
mkTypedSessionVar columnType =
  PSESessVar (columnTypeToScalarType <$> columnType)

columnTypeToScalarType :: RQL.T.C.ColumnType 'DataConnector -> DC.ScalarType
columnTypeToScalarType = \case
  RQL.T.C.ColumnScalar scalarType -> scalarType
  -- NOTE: This should be unreachable:
  RQL.T.C.ColumnEnumReference _ -> DC.StringTy
