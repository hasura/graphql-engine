{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataWrapper.Adapter.Metadata () where

--------------------------------------------------------------------------------

import Data.Aeson qualified as J
import Data.Environment (Environment)
import Data.HashMap.Strict qualified as Map
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text.Extended (toTxt)
import Hasura.Backends.DataWrapper.API qualified as API
import Hasura.Backends.DataWrapper.Adapter.Types qualified as GDW
  ( SourceConfig (..),
  )
import Hasura.Backends.DataWrapper.Agent.Client qualified as Agent.Client
import Hasura.Backends.DataWrapper.IR.Expression qualified as IR
import Hasura.Backends.DataWrapper.IR.Name qualified as IR
import Hasura.Backends.DataWrapper.IR.Scalar.Type qualified as IR.Scalar
import Hasura.Backends.DataWrapper.IR.Table qualified as IR.Table
import Hasura.Backends.Postgres.SQL.Types (PGDescription (..))
import Hasura.Base.Error (Code (..), QErr, throw400, withPathK)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (OpExpG (..), PartialSQLExp (..))
import Hasura.RQL.Types.Column (ColumnMutability (..), ColumnReference, ColumnType (..), RawColumnInfo (..), ValueParser, columnReferenceType, parseScalarValueColumnType)
import Hasura.RQL.Types.Common (OID (..), SourceName)
import Hasura.RQL.Types.Metadata (SourceMetadata (..))
import Hasura.RQL.Types.Metadata.Backend (BackendMetadata (..))
import Hasura.RQL.Types.Source (ResolvedSource (..))
import Hasura.RQL.Types.SourceCustomization (SourceTypeCustomization)
import Hasura.RQL.Types.Table (Constraint (..), DBTableMetadata (..), FieldInfo, FieldInfoMap, PrimaryKey (..), ViewInfo (..))
import Hasura.SQL.Backend (BackendType (..))
import Hasura.SQL.Types (CollectableType (..))
import Hasura.Server.Utils (isReqUserId, isSessionVariable, userIdHeader)
import Hasura.Session (SessionVariable, mkSessionVariable)
import Hasura.Tracing (noReporter, runTraceTWithReporter)
import Language.GraphQL.Draft.Syntax qualified as GQL
import Network.HTTP.Client qualified as HTTP
import Witch qualified

--------------------------------------------------------------------------------

instance BackendMetadata 'DataWrapper where
  resolveSourceConfig = resolveSourceConfig'
  resolveDatabaseMetadata = resolveDatabaseMetadata'
  parseBoolExpOperations = parseBoolExpOperations'
  parseCollectableType = parseCollectableType'
  buildComputedFieldInfo = error "buildComputedFieldInfo: not implemented for GraphQL Data Wrappers."
  fetchAndValidateEnumValues = error "fetchAndValidateEnumValues: not implemented for GraphQL Data Wrappers."
  buildFunctionInfo = error "buildFunctionInfo: not implemented for GraphQL Data Wrappers."
  updateColumnInEventTrigger = error "updateColumnInEventTrigger: not implemented for GraphQL Data Wrappers."
  postDropSourceHook = error "postDropSourceHook: not implemented for GraphQL Data Wrappers."

resolveSourceConfig' ::
  MonadIO m =>
  SourceName ->
  Agent.Client.ConnSourceConfig ->
  Environment ->
  m (Either QErr GDW.SourceConfig)
resolveSourceConfig' _ (Agent.Client.ConnSourceConfig endpoint) _ = runExceptT do
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  API.Routes {..} <- liftIO $ Agent.Client.client manager (Agent.Client.ConnSourceConfig endpoint)
  schemaResponse <- runTraceTWithReporter noReporter "resolve source" _schema
  pure
    GDW.SourceConfig
      { dscEndpoint = endpoint,
        dscSchema = schemaResponse,
        dscManager = manager
      }

resolveDatabaseMetadata' ::
  Applicative m =>
  SourceMetadata 'DataWrapper ->
  GDW.SourceConfig ->
  SourceTypeCustomization ->
  m (Either QErr (ResolvedSource 'DataWrapper))
resolveDatabaseMetadata' _ sc@(GDW.SourceConfig _ (API.SchemaResponse {..}) _) customization =
  let tables = Map.fromList $ do
        API.TableInfo {..} <- srTables
        let meta =
              DBTableMetadata
                { _ptmiOid = OID 0,
                  _ptmiColumns = do
                    API.ColumnInfo {..} <- dtiColumns
                    pure $
                      RawColumnInfo
                        { rciName = Witch.from dciName,
                          rciPosition = 1,
                          rciType = Witch.from dciType,
                          rciIsNullable = dciNullable,
                          rciDescription = fmap GQL.Description dciDescription,
                          -- TODO: Add Column Mutability to the 'TableInfo'
                          rciMutability = ColumnMutability False False
                        },
                  _ptmiPrimaryKey = dtiPrimaryKey <&> \key -> PrimaryKey (Constraint () (OID 0)) (NESeq.singleton (coerce key)),
                  _ptmiUniqueConstraints = mempty,
                  _ptmiForeignKeys = mempty,
                  _ptmiViewInfo = Just $ ViewInfo False False False,
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
  ValueParser 'DataWrapper m v ->
  IR.Table.Name ->
  FieldInfoMap (FieldInfo 'DataWrapper) ->
  ColumnReference 'DataWrapper ->
  J.Value ->
  m [OpExpG 'DataWrapper v]
parseBoolExpOperations' rhsParser _table _fields columnRef value =
  withPathK (toTxt columnRef) $ parseOperations (columnReferenceType columnRef) value
  where
    parseWithTy ty = rhsParser (CollectableTypeScalar ty)

    parseOperations :: ColumnType 'DataWrapper -> J.Value -> m [OpExpG 'DataWrapper v]
    parseOperations columnType = \case
      J.Object o -> mapM (parseOperation columnType) $ Map.toList o
      v -> pure . AEQ False <$> parseWithTy columnType v

    parseOperation :: ColumnType 'DataWrapper -> (Text, J.Value) -> m (OpExpG 'DataWrapper v)
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
        -- "$in"            -> parseIn
        -- "_in"            -> parseIn
        --
        -- "$nin"           -> parseNin
        -- "_nin"           -> parseNin

        -- "$like"          -> parseLike
        -- "_like"          -> parseLike
        --
        -- "$nlike"         -> parseNlike
        -- "_nlike"         -> parseNlike

        x -> throw400 UnexpectedPayload $ "Unknown operator : " <> x
      where
        -- colTy = columnReferenceType columnRef

        parseOne = parseWithTy columnType val
        -- parseManyWithType ty = rhsParser (CollectableTypeArray ty) val

        parseEq = AEQ False <$> parseOne
        parseNeq = ANE False <$> parseOne
        -- parseIn    = AIN <$> parseManyWithType colTy
        -- parseNin   = ANIN <$> parseManyWithType colTy
        parseGt = AGT <$> parseOne
        parseLt = ALT <$> parseOne
        parseGte = AGTE <$> parseOne
        parseLte = ALTE <$> parseOne

parseCollectableType' ::
  MonadError QErr m =>
  CollectableType (ColumnType 'DataWrapper) ->
  J.Value ->
  m (PartialSQLExp 'DataWrapper)
parseCollectableType' collectableType = \case
  J.String t
    | isSessionVariable t -> pure $ mkTypedSessionVar collectableType $ mkSessionVariable t
    | isReqUserId t -> pure $ mkTypedSessionVar collectableType userIdHeader
  val -> case collectableType of
    CollectableTypeScalar scalarType ->
      PSESQLExp . IR.Literal <$> parseScalarValueColumnType scalarType val
    CollectableTypeArray _ ->
      throw400 NotSupported "Array types are not supported by dynamic backends"

mkTypedSessionVar ::
  CollectableType (ColumnType 'DataWrapper) ->
  SessionVariable ->
  PartialSQLExp 'DataWrapper
mkTypedSessionVar columnType =
  PSESessVar (columnTypeToScalarType <$> columnType)

columnTypeToScalarType :: ColumnType 'DataWrapper -> IR.Scalar.Type
columnTypeToScalarType = \case
  ColumnScalar scalarType -> scalarType
  ColumnEnumReference _ -> IR.Scalar.String -- is this even reachable?
