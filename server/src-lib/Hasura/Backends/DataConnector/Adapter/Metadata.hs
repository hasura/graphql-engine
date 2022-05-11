{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataConnector.Adapter.Metadata () where

import Data.Aeson qualified as J
import Data.Environment (Environment)
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as Text
import Data.Text.Extended (toTxt, (<<>))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC
import Hasura.Backends.DataConnector.Agent.Client qualified as Agent.Client
import Hasura.Backends.DataConnector.IR.Expression qualified as IR.E
import Hasura.Backends.DataConnector.IR.Name qualified as IR.N
import Hasura.Backends.DataConnector.IR.Scalar.Type qualified as IR.S.T
import Hasura.Backends.DataConnector.IR.Table qualified as IR.T
import Hasura.Backends.Postgres.SQL.Types (PGDescription (..))
import Hasura.Base.Error (Code (..), QErr, throw400, withPathK)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (OpExpG (..), PartialSQLExp (..))
import Hasura.RQL.Types.Column qualified as RQL.T.C
import Hasura.RQL.Types.Common (OID (..), SourceName)
import Hasura.RQL.Types.EventTrigger (RecreateEventTriggers (RETDoNothing))
import Hasura.RQL.Types.Metadata (SourceMetadata (..))
import Hasura.RQL.Types.Metadata.Backend (BackendMetadata (..))
import Hasura.RQL.Types.Source (ResolvedSource (..))
import Hasura.RQL.Types.SourceCustomization (SourceTypeCustomization)
import Hasura.RQL.Types.Table qualified as RQL.T.T
import Hasura.SQL.Backend (BackendSourceKind (..), BackendType (..))
import Hasura.SQL.Types (CollectableType (..))
import Hasura.Server.Utils qualified as HSU
import Hasura.Session (SessionVariable, mkSessionVariable)
import Hasura.Tracing (TraceT, noReporter, runTraceTWithReporter)
import Language.GraphQL.Draft.Syntax qualified as GQL
import Network.HTTP.Client qualified as HTTP
import Servant.Client (AsClientT)
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
  postDropSourceHook _sourceConfig = pure ()

resolveSourceConfig' ::
  MonadIO m =>
  SourceName ->
  DC.ConnSourceConfig ->
  BackendSourceKind 'DataConnector ->
  DC.DataConnectorBackendConfig ->
  Environment ->
  m (Either QErr DC.SourceConfig)
resolveSourceConfig' sourceName (DC.ConnSourceConfig config) (DataConnectorKind dataConnectorName) backendConfig _ = runExceptT do
  DC.DataConnectorOptions {..} <-
    OMap.lookup dataConnectorName backendConfig
      `onNothing` throw400 DataConnectorError ("Data connector named " <> toTxt dataConnectorName <> " was not found in the data connector backend config")
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  routes@API.Routes {..} <- liftIO $ Agent.Client.client manager _dcoUri
  schemaResponse <- runTraceTWithReporter noReporter "resolve source" $ do
    validateConfiguration routes sourceName dataConnectorName config
    _schema config
  pure
    DC.SourceConfig
      { _scEndpoint = _dcoUri,
        _scConfig = config,
        _scSchema = schemaResponse,
        _scManager = manager
      }

validateConfiguration ::
  MonadIO m =>
  API.Routes (AsClientT (TraceT (ExceptT QErr m))) ->
  SourceName ->
  DC.DataConnectorName ->
  API.Config ->
  TraceT (ExceptT QErr m) ()
validateConfiguration API.Routes {..} sourceName dataConnectorName config = do
  configSchemaResponse <- _configSchema
  let errors = API.validateConfigAgainstConfigSchema configSchemaResponse config
  if errors /= []
    then
      let errorsText = Text.unlines (("- " <>) . Text.pack <$> errors)
       in throw400
            DataConnectorError
            ("Configuration for source " <> sourceName <<> " is not valid based on the configuration schema declared by the " <> dataConnectorName <<> " data connector agent. Errors:\n" <> errorsText)
    else pure ()

resolveDatabaseMetadata' ::
  Applicative m =>
  SourceMetadata 'DataConnector ->
  DC.SourceConfig ->
  SourceTypeCustomization ->
  m (Either QErr (ResolvedSource 'DataConnector))
resolveDatabaseMetadata' _ sc@(DC.SourceConfig {_scSchema = API.SchemaResponse {..}}) customization =
  let tables = Map.fromList $ do
        API.TableInfo {..} <- srTables
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
                  _ptmiPrimaryKey = dtiPrimaryKey <&> \key -> RQL.T.T.PrimaryKey (RQL.T.T.Constraint () (OID 0)) (NESeq.singleton (coerce key)),
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
              _rsPgScalars = mempty
            }

-- | This is needed to get permissions to work
parseBoolExpOperations' ::
  forall m v.
  MonadError QErr m =>
  RQL.T.C.ValueParser 'DataConnector m v ->
  IR.T.Name ->
  RQL.T.T.FieldInfoMap (RQL.T.T.FieldInfo 'DataConnector) ->
  RQL.T.C.ColumnReference 'DataConnector ->
  J.Value ->
  m [OpExpG 'DataConnector v]
parseBoolExpOperations' rhsParser _table _fields columnRef value =
  withPathK (toTxt columnRef) $ parseOperations (RQL.T.C.columnReferenceType columnRef) value
  where
    parseWithTy ty = rhsParser (CollectableTypeScalar ty)

    parseOperations :: RQL.T.C.ColumnType 'DataConnector -> J.Value -> m [OpExpG 'DataConnector v]
    parseOperations columnType = \case
      J.Object o -> traverse (parseOperation columnType) $ Map.toList o
      v -> pure . AEQ False <$> parseWithTy columnType v

    parseOperation :: RQL.T.C.ColumnType 'DataConnector -> (Text, J.Value) -> m (OpExpG 'DataConnector v)
    parseOperation columnType (opStr, val) = withPathK opStr $
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
        "$in" -> parseIn
        "_in" -> parseIn
        "$nin" -> parseNin
        "_nin" -> parseNin
        -- "$like"          -> parseLike
        -- "_like"          -> parseLike
        --
        -- "$nlike"         -> parseNlike
        -- "_nlike"         -> parseNlike

        x -> throw400 UnexpectedPayload $ "Unknown operator : " <> x
      where
        colTy = RQL.T.C.columnReferenceType columnRef

        parseOne = parseWithTy columnType val
        parseManyWithType ty = rhsParser (CollectableTypeArray ty) val

        parseEq = AEQ False <$> parseOne
        parseNeq = ANE False <$> parseOne
        parseIn = AIN <$> parseManyWithType colTy
        parseNin = ANIN <$> parseManyWithType colTy
        parseGt = AGT <$> parseOne
        parseLt = ALT <$> parseOne
        parseGte = AGTE <$> parseOne
        parseLte = ALTE <$> parseOne

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
      PSESQLExp . IR.E.Literal <$> RQL.T.C.parseScalarValueColumnType scalarType val
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
