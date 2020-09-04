module Hasura.RQL.Types.Action.Class
  ( InsertActionTx
  , SetCompletedTx
  , SetErrorTx
  , AsyncActionQueryResolver
  , MonadAsyncActions(..)
  , processOutputSelectionSet
  , mkJsonAggSelect
  )
where

import           Hasura.Db
import           Hasura.GraphQL.Parser          hiding (column)
import           Hasura.Prelude
import           Hasura.RQL.Types.Action
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.CustomTypes
import           Hasura.RQL.Types.Error
import           Hasura.Session
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.RQL.DML.Select.Internal as RS

import qualified Data.Aeson                     as J
import qualified Data.CaseInsensitive           as CI
import qualified Data.HashMap.Strict            as Map
import qualified Database.PG.Query              as Q
import qualified Hasura.Tracing                 as Tracing
import qualified Language.GraphQL.Draft.Syntax  as G
import qualified Network.HTTP.Types             as HTTP

type InsertActionTx = ActionName -> SessionVariables -> [HTTP.Header] -> J.Value -> Q.TxE QErr ActionId
type SetCompletedTx = ActionId -> J.Value -> Q.TxE QErr ()
type SetErrorTx = ActionId -> QErr -> Q.TxE QErr ()
type AsyncActionQueryResolver = UserInfo -> AnnActionAsyncQuery UnpreparedValue -> RS.AnnSimpleSelG UnpreparedValue

class (Monad m) => MonadAsyncActions m where
  getInsertActionTx :: m InsertActionTx
  default getInsertActionTx :: m InsertActionTx
  getInsertActionTx = pure insertActionTx

  getUndeliveredEventsTx :: m (Q.TxE QErr [ActionLogItem])
  default getUndeliveredEventsTx :: m (Q.TxE QErr [ActionLogItem])
  getUndeliveredEventsTx = pure undeliveredEventsTx

  getSetCompletedTx :: m SetCompletedTx
  default getSetCompletedTx :: m SetCompletedTx
  getSetCompletedTx = pure setCompletedTx

  getSetErrorTx :: m SetErrorTx
  default getSetErrorTx :: m SetErrorTx
  getSetErrorTx = pure setErrorTx

  getAsyncActionQueryResolver :: m AsyncActionQueryResolver
  default getAsyncActionQueryResolver :: m AsyncActionQueryResolver
  getAsyncActionQueryResolver = pure resolveAsyncActionQuery

instance (MonadAsyncActions m) => MonadAsyncActions (Tracing.TraceT m) where
  getInsertActionTx      = lift getInsertActionTx
  getUndeliveredEventsTx = lift getUndeliveredEventsTx
  getSetCompletedTx      = lift getSetCompletedTx
  getSetErrorTx          = lift getSetErrorTx

instance (MonadAsyncActions m) => MonadAsyncActions (ExceptT e m) where
  getInsertActionTx      = lift getInsertActionTx
  getUndeliveredEventsTx = lift getUndeliveredEventsTx
  getSetCompletedTx      = lift getSetCompletedTx
  getSetErrorTx          = lift getSetErrorTx

instance (MonadAsyncActions m) => MonadAsyncActions (ReaderT r m) where
  getInsertActionTx      = lift getInsertActionTx
  getUndeliveredEventsTx = lift getUndeliveredEventsTx
  getSetCompletedTx      = lift getSetCompletedTx
  getSetErrorTx          = lift getSetErrorTx

insertActionTx :: InsertActionTx
insertActionTx actionName sessionVariables httpHeaders inputArgsPayload =
  runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler [Q.sql|
    INSERT INTO
        "hdb_catalog"."hdb_action_log"
        ("action_name", "session_variables", "request_headers", "input_payload", "status")
    VALUES
        ($1, $2, $3, $4, $5)
    RETURNING "id"
   |]
    ( actionName
    , Q.AltJ sessionVariables
    , Q.AltJ $ toHeadersMap httpHeaders
    , Q.AltJ inputArgsPayload
    , "created"::Text
    ) False
  where
    toHeadersMap = Map.fromList . map ((bsToTxt . CI.original) *** bsToTxt)

undeliveredEventsTx :: Q.TxE QErr [ActionLogItem]
undeliveredEventsTx =
  map mapEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
    update hdb_catalog.hdb_action_log set status = 'processing'
    where
      id in (
        select id from hdb_catalog.hdb_action_log
        where status = 'created'
        for update skip locked limit 10
      )
    returning
      id, action_name, request_headers::json, session_variables::json, input_payload::json
  |] () False
 where
   mapEvent (actionId, actionName, Q.AltJ headersMap,
             Q.AltJ sessionVariables, Q.AltJ inputPayload) =
     ActionLogItem actionId actionName (fromHeadersMap headersMap) sessionVariables inputPayload

   fromHeadersMap = map ((CI.mk . txtToBs) *** txtToBs) . Map.toList

setCompletedTx :: SetCompletedTx
setCompletedTx actionId responsePayload =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    update hdb_catalog.hdb_action_log
    set response_payload = $1, status = 'completed'
    where id = $2
  |] (Q.AltJ responsePayload, actionId) False

setErrorTx :: SetErrorTx
setErrorTx actionId e =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    update hdb_catalog.hdb_action_log
    set errors = $1, status = 'error'
    where id = $2
  |] (Q.AltJ e, actionId) False

-- TODO: Add tracing here? Avoided now because currently the function is pure
resolveAsyncActionQuery :: AsyncActionQueryResolver
resolveAsyncActionQuery userInfo annAction =
  let annotatedFields = asyncFields <&> second \case
        AsyncTypename t -> RS.AFExpression t
        AsyncOutput annFields ->
          -- See Note [Resolving async action query/subscription]
          let inputTableArgument = RS.AETableRow $ Just $ Iden "response_payload"
              jsonAggSelect = mkJsonAggSelect outputType
          in RS.AFComputedField $ RS.CFSTable jsonAggSelect $
             processOutputSelectionSet inputTableArgument outputType
             definitionList annFields stringifyNumerics

        AsyncId        -> mkAnnFldFromPGCol "id" PGUUID
        AsyncCreatedAt -> mkAnnFldFromPGCol "created_at" PGTimeStampTZ
        AsyncErrors    -> mkAnnFldFromPGCol "errors" PGJSONB

      tableFromExp = RS.FromTable actionLogTable
      tableArguments = RS.noSelectArgs
                       { RS._saWhere = Just tableBoolExpression}
      tablePermissions = RS.TablePerm annBoolExpTrue Nothing

  in RS.AnnSelectG annotatedFields tableFromExp tablePermissions
     tableArguments stringifyNumerics
  where
    AnnActionAsyncQuery _ actionId outputType asyncFields definitionList stringifyNumerics = annAction
    actionLogTable = QualifiedObject (SchemaName "hdb_catalog") (TableName "hdb_action_log")

    -- TODO (from master):- Avoid using PGColumnInfo
    mkAnnFldFromPGCol column' columnType =
      flip RS.mkAnnColumnField Nothing $
      PGColumnInfo (unsafePGCol column') (G.unsafeMkName column') 0 (PGColumnScalar columnType) True Nothing

    tableBoolExpression =
      let actionIdColumnInfo = PGColumnInfo (unsafePGCol "id") $$(G.litName "id")
                               0 (PGColumnScalar PGUUID) False Nothing
          actionIdColumnEq = BoolFld $ AVCol actionIdColumnInfo [AEQ True actionId]
          sessionVarsColumnInfo = PGColumnInfo (unsafePGCol "session_variables") $$(G.litName "session_variables")
                                  0 (PGColumnScalar PGJSONB) False Nothing
          sessionVarValue = flip UVParameter Nothing $ PGColumnValue (PGColumnScalar PGJSONB) $
                            WithScalarType PGJSONB $ PGValJSONB $ Q.JSONB $ J.toJSON $ _uiSession userInfo
          sessionVarsColumnEq = BoolFld $ AVCol sessionVarsColumnInfo [AEQ True sessionVarValue]

      -- For non-admin roles, accessing an async action's response should be allowed only for the user
      -- who initiated the action through mutation. The action's response is accessible for a query/subscription
      -- only when it's session variables are equal to that of action's.
      in if isAdmin (_uiRole userInfo) then actionIdColumnEq
         else BoolAnd [actionIdColumnEq, sessionVarsColumnEq]

processOutputSelectionSet
  :: RS.ArgumentExp v
  -> GraphQLType
  -> [(PGCol, PGScalarType)]
  -> RS.AnnFieldsG v
  -> Bool
  -> RS.AnnSimpleSelG v
processOutputSelectionSet tableRowInput actionOutputType definitionList annotatedFields =
  RS.AnnSelectG annotatedFields selectFrom RS.noTablePermissions RS.noSelectArgs
  where
    jsonbToPostgresRecordFunction =
      QualifiedObject "pg_catalog" $ FunctionName $
      if isListType actionOutputType then
        "jsonb_to_recordset" -- Multirow array response
      else "jsonb_to_record" -- Single object response

    functionArgs = RS.FunctionArgsExp [tableRowInput] mempty
    selectFrom = RS.FromFunction jsonbToPostgresRecordFunction functionArgs $ Just definitionList

mkJsonAggSelect :: GraphQLType -> RS.JsonAggSelect
mkJsonAggSelect =
  bool RS.JASSingleObject RS.JASMultipleRows . isListType
