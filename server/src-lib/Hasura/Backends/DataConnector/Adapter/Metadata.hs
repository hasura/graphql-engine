{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataConnector.Adapter.Metadata () where

import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Environment (Environment)
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as Text
import Data.Text.Extended (toTxt, (<<>), (<>>))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.ConfigTransform (transformConnSourceConfig)
import Hasura.Backends.DataConnector.Adapter.Types (ConnSourceConfig (ConnSourceConfig))
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC
import Hasura.Backends.DataConnector.Agent.Client (AgentClientContext (..), runAgentClientT)
import Hasura.Backends.DataConnector.IR.Column qualified as IR.C
import Hasura.Backends.DataConnector.IR.Name qualified as IR.N
import Hasura.Backends.DataConnector.IR.Scalar.Type qualified as IR.S.T
import Hasura.Backends.DataConnector.IR.Scalar.Value qualified as IR.S.V
import Hasura.Backends.DataConnector.IR.Table qualified as IR.T
import Hasura.Backends.Postgres.SQL.Types (PGDescription (..))
import Hasura.Base.Error (Code (..), QErr, decodeValue, throw400, throw500, withPathK)
import Hasura.Logging (Hasura, Logger)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (OpExpG (..), PartialSQLExp (..), RootOrCurrent (..), RootOrCurrentColumn (..))
import Hasura.RQL.Types.Column qualified as RQL.T.C
import Hasura.RQL.Types.Common (OID (..), SourceName)
import Hasura.RQL.Types.EventTrigger (RecreateEventTriggers (RETDoNothing))
import Hasura.RQL.Types.Metadata (SourceMetadata (..))
import Hasura.RQL.Types.Metadata.Backend (BackendMetadata (..))
import Hasura.RQL.Types.SchemaCache qualified as SchemaCache
import Hasura.RQL.Types.Source (ResolvedSource (..))
import Hasura.RQL.Types.SourceCustomization (SourceTypeCustomization)
import Hasura.RQL.Types.Table qualified as RQL.T.T
import Hasura.SQL.Backend (BackendSourceKind (..), BackendType (..))
import Hasura.SQL.Types (CollectableType (..))
import Hasura.Server.Utils qualified as HSU
import Hasura.Session (SessionVariable, mkSessionVariable)
import Hasura.Tracing (noReporter, runTraceTWithReporter)
import Language.GraphQL.Draft.Syntax qualified as GQL
import Network.HTTP.Client qualified as HTTP
import Servant.Client.Core.HasClient ((//))
import Servant.Client.Generic (genericClient)
import Witch qualified

instance BackendMetadata 'DataConnector where
  prepareCatalog = const $ pure RETDoNothing
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

resolveSourceConfig' ::
  MonadIO m =>
  Logger Hasura ->
  SourceName ->
  DC.ConnSourceConfig ->
  BackendSourceKind 'DataConnector ->
  DC.DataConnectorBackendConfig ->
  Environment ->
  m (Either QErr DC.SourceConfig)
resolveSourceConfig' logger sourceName csc@ConnSourceConfig {template, value = originalConfig} (DataConnectorKind dataConnectorName) backendConfig env = runExceptT do
  DC.DataConnectorOptions {..} <-
    OMap.lookup dataConnectorName backendConfig
      `onNothing` throw400 DataConnectorError ("Data connector named " <> toTxt dataConnectorName <> " was not found in the data connector backend config")

  transformedConfig <- transformConnSourceConfig csc [("$session", J.object []), ("$env", J.toJSON env)] env
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings

  -- TODO: capabilities applies to all sources for an agent.
  -- We should be able to call it once per agent and store it in the SchemaCache
  API.CapabilitiesResponse {..} <-
    runTraceTWithReporter noReporter "capabilities"
      . flip runAgentClientT (AgentClientContext logger _dcoUri manager)
      $ genericClient // API._capabilities

  validateConfiguration sourceName dataConnectorName crConfigSchemaResponse transformedConfig

  schemaResponse <-
    runTraceTWithReporter noReporter "resolve source"
      . flip runAgentClientT (AgentClientContext logger _dcoUri manager)
      $ (genericClient // API._schema) (toTxt sourceName) transformedConfig

  pure
    DC.SourceConfig
      { _scEndpoint = _dcoUri,
        _scConfig = originalConfig,
        _scTemplate = template,
        _scCapabilities = crCapabilities,
        _scSchema = schemaResponse,
        _scManager = manager,
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
  let tables = Map.fromList $ do
        API.TableInfo {..} <- srTables
        let primaryKeyColumns = Seq.fromList $ coerce <$> fromMaybe [] dtiPrimaryKey
        let meta =
              RQL.T.T.DBTableMetadata
                { _ptmiOid = OID 0,
                  _ptmiColumns = do
                    API.ColumnInfo {..} <- dtiColumns
                    pure $
                      RQL.T.C.RawColumnInfo
                        { rciName = Witch.from dciName,
                          rciPosition = 1,
                          rciType = Witch.from dciType,
                          rciIsNullable = dciNullable,
                          rciDescription = fmap GQL.Description dciDescription,
                          -- TODO: Add Column Mutability to the 'TableInfo'
                          rciMutability = RQL.T.C.ColumnMutability False False
                        },
                  _ptmiPrimaryKey = RQL.T.T.PrimaryKey (RQL.T.T.Constraint () (OID 0)) <$> NESeq.nonEmptySeq primaryKeyColumns,
                  _ptmiUniqueConstraints = mempty,
                  _ptmiForeignKeys = mempty,
                  _ptmiViewInfo = Just $ RQL.T.T.ViewInfo False False False,
                  _ptmiDescription = fmap PGDescription dtiDescription,
                  _ptmiExtraTableMetadata = ()
                }
        pure (coerce dtiName, meta)
   in pure $
        pure $
          ResolvedSource
            { _rsConfig = sc,
              _rsCustomization = customization,
              _rsTables = tables,
              _rsFunctions = mempty,
              _rsScalars = mempty
            }

-- | This is needed to get permissions to work
parseBoolExpOperations' ::
  forall m v.
  (MonadError QErr m, SchemaCache.TableCoreInfoRM 'DataConnector m) =>
  RQL.T.C.ValueParser 'DataConnector m v ->
  IR.T.Name ->
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

        x -> throw400 UnexpectedPayload $ "Unknown operator : " <> x
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

        validateRhsColumn :: RQL.T.T.FieldInfoMap (RQL.T.T.FieldInfo 'DataConnector) -> IR.C.Name -> m IR.C.Name
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
      PSESQLExp . IR.S.V.ValueLiteral <$> RQL.T.C.parseScalarValueColumnType scalarType val
    CollectableTypeArray _ ->
      throw400 NotSupported "Array types are not supported by the Data Connector backend"

mkTypedSessionVar ::
  CollectableType (RQL.T.C.ColumnType 'DataConnector) ->
  SessionVariable ->
  PartialSQLExp 'DataConnector
mkTypedSessionVar columnType =
  PSESessVar (columnTypeToScalarType <$> columnType)

columnTypeToScalarType :: RQL.T.C.ColumnType 'DataConnector -> IR.S.T.Type
columnTypeToScalarType = \case
  RQL.T.C.ColumnScalar scalarType -> scalarType
  -- NOTE: This should be unreachable:
  RQL.T.C.ColumnEnumReference _ -> IR.S.T.String
