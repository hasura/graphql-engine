{-# LANGUAGE TemplateHaskell #-}

-- | Postgres Execute Mutation
--
-- Generic combinators for translating and excecuting IR mutation statements.
-- Used by the specific mutation modules, e.g. 'Hasura.Backends.Postgres.Execute.Insert'.
--
-- See 'Hasura.Backends.Postgres.Instances.Execute'.
module Hasura.Backends.Postgres.Execute.Mutation
  ( MutateResp (..),
    --
    execDeleteQuery,
    execInsertQuery,
    execUpdateQuery,
    --
    executeMutationOutputQuery,
    mutateAndFetchCols,
    --
    ValidateInputPayloadVersion,
    validateInputPayloadVersion,
    ValidateInputErrorResponse (..),
    HttpHandlerLog (..),
    ValidateInsertInputLog (..),
    InsertValidationPayloadMap,
    validateUpdateMutation,
    validateDeleteMutation,
    validateMutation,
  )
where

import Control.Exception (try)
import Control.Lens qualified as Lens
import Data.Aeson
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as J
import Data.Aeson.TH qualified as J
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Sequence qualified as DS
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types hiding (TableName)
import Hasura.Backends.Postgres.SQL.Value
import Hasura.Backends.Postgres.Translate.Delete
import Hasura.Backends.Postgres.Translate.Insert
import Hasura.Backends.Postgres.Translate.Mutation
import Hasura.Backends.Postgres.Translate.Returning
import Hasura.Backends.Postgres.Translate.Select
import Hasura.Backends.Postgres.Translate.Select.Internal.Helpers (customSQLToTopLevelCTEs, toQuery)
import Hasura.Backends.Postgres.Translate.Update
import Hasura.Backends.Postgres.Types.Update qualified as Postgres
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Parser.Internal.Convert
import Hasura.GraphQL.Parser.Variable qualified as G
import Hasura.HTTP
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.DDL.Headers
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Delete
import Hasura.RQL.IR.Insert
import Hasura.RQL.IR.Returning
import Hasura.RQL.IR.Select
import Hasura.RQL.IR.Update
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Headers (HeaderConf)
import Hasura.RQL.Types.NamingCase (NamingCase)
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.Server.Utils
import Hasura.Session
import Hasura.Tracing (b3TraceContextPropagator)
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Client.Transformable qualified as HTTP
import Network.Wreq qualified as Wreq

data MutateResp (b :: BackendType) a = MutateResp
  { _mrAffectedRows :: Int,
    _mrReturningColumns :: [ColumnValues b a]
  }
  deriving (Generic)

deriving instance (Backend b, Show a) => Show (MutateResp b a)

deriving instance (Backend b, Eq a) => Eq (MutateResp b a)

instance (Backend b, ToJSON a) => ToJSON (MutateResp b a) where
  toJSON = genericToJSON hasuraJSON

instance (Backend b, FromJSON a) => FromJSON (MutateResp b a) where
  parseJSON = genericParseJSON hasuraJSON

data Mutation (b :: BackendType) = Mutation
  { _mTable :: QualifiedTable,
    _mQuery :: (MutationCTE, DS.Seq PG.PrepArg),
    _mOutput :: MutationOutput b,
    _mCols :: [ColumnInfo b],
    _mStrfyNum :: Options.StringifyNumbers,
    _mNamingConvention :: Maybe NamingCase
  }

mkMutation ::
  UserInfo ->
  QualifiedTable ->
  (MutationCTE, DS.Seq PG.PrepArg) ->
  MutationOutput ('Postgres pgKind) ->
  [ColumnInfo ('Postgres pgKind)] ->
  Options.StringifyNumbers ->
  Maybe NamingCase ->
  Mutation ('Postgres pgKind)
mkMutation _userInfo table query output allCols strfyNum tCase =
  Mutation table query output allCols strfyNum tCase

runMutation ::
  ( MonadTx m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m,
    MonadIO m
  ) =>
  UserInfo ->
  Mutation ('Postgres pgKind) ->
  m EncJSON
runMutation userInfo mut =
  bool (mutateAndReturn userInfo mut) (mutateAndSel userInfo mut)
    $ hasNestedFld
    $ _mOutput mut

mutateAndReturn ::
  ( MonadTx m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m,
    MonadIO m
  ) =>
  UserInfo ->
  Mutation ('Postgres pgKind) ->
  m EncJSON
mutateAndReturn userInfo (Mutation qt (cte, p) mutationOutput allCols strfyNum tCase) =
  executeMutationOutputQuery userInfo qt allCols Nothing cte mutationOutput strfyNum tCase (toList p)

execUpdateQuery ::
  forall pgKind m.
  ( MonadTx m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m,
    MonadIO m
  ) =>
  Options.StringifyNumbers ->
  Maybe NamingCase ->
  UserInfo ->
  (AnnotatedUpdate ('Postgres pgKind), DS.Seq PG.PrepArg) ->
  m EncJSON
execUpdateQuery strfyNum tCase userInfo (u, p) = do
  updateCTE <- mkUpdateCTE userInfo u
  case updateCTE of
    Update singleUpdate -> runCTE singleUpdate
    MultiUpdate ctes -> encJFromList <$> traverse runCTE ctes
  where
    runCTE :: S.TopLevelCTE -> m EncJSON
    runCTE cte =
      runMutation
        userInfo
        (mkMutation userInfo (_auTable u) (MCCheckConstraint cte, p) (_auOutput u) (_auAllCols u) strfyNum tCase)

execDeleteQuery ::
  forall pgKind m.
  ( MonadTx m,
    MonadIO m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m
  ) =>
  Options.StringifyNumbers ->
  Maybe NamingCase ->
  UserInfo ->
  (AnnDel ('Postgres pgKind), DS.Seq PG.PrepArg) ->
  m EncJSON
execDeleteQuery strfyNum tCase userInfo (u, p) = do
  delete <- mkDelete userInfo u
  runMutation
    userInfo
    (mkMutation userInfo (_adTable u) (MCDelete delete, p) (_adOutput u) (_adAllCols u) strfyNum tCase)

execInsertQuery ::
  ( MonadTx m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m,
    MonadIO m
  ) =>
  Options.StringifyNumbers ->
  Maybe NamingCase ->
  UserInfo ->
  (InsertQueryP1 ('Postgres pgKind), DS.Seq PG.PrepArg) ->
  m EncJSON
execInsertQuery strfyNum tCase userInfo (u, p) = do
  insertCTE <- mkInsertCTE userInfo u
  runMutation
    userInfo
    (mkMutation userInfo (iqp1Table u) (MCCheckConstraint insertCTE, p) (iqp1Output u) (iqp1AllCols u) strfyNum tCase)

{- Note: [Prepared statements in Mutations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The SQL statements we generate for mutations seem to include the actual values
in the statements in some cases which pretty much makes them unfit for reuse
(Handling relationships in the returning clause is the source of this
complexity). Further, `PGConn` has an internal cache which maps a statement to
a 'prepared statement id' on Postgres. As we prepare more and more single-use
SQL statements we end up leaking memory both on graphql-engine and Postgres
till the connection is closed. So a simpler but very crude fix is to not use
prepared statements for mutations. The performance of insert mutations
shouldn't be affected but updates and delete mutations with complex boolean
conditions **might** see some degradation.
-}

mutateAndSel ::
  forall pgKind m.
  ( MonadTx m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m,
    MonadIO m
  ) =>
  UserInfo ->
  Mutation ('Postgres pgKind) ->
  m EncJSON
mutateAndSel userInfo (Mutation qt q mutationOutput allCols strfyNum tCase) = do
  -- Perform mutation and fetch unique columns
  MutateResp _ columnVals <- liftTx $ mutateAndFetchCols userInfo qt allCols q strfyNum tCase
  select <- mkSelectExpFromColumnValues qt allCols columnVals
  -- Perform select query and fetch returning fields
  executeMutationOutputQuery
    userInfo
    qt
    allCols
    Nothing
    (MCSelectValues select)
    mutationOutput
    strfyNum
    tCase
    []

withCheckPermission :: (MonadError QErr m) => m (a, Bool) -> m a
withCheckPermission sqlTx = do
  (rawResponse, checkConstraint) <- sqlTx
  unless checkConstraint
    $ throw400 PermissionError
    $ "check constraint of an insert/update permission has failed"
  pure rawResponse

executeMutationOutputQuery ::
  forall pgKind m.
  ( MonadTx m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m,
    MonadIO m
  ) =>
  UserInfo ->
  QualifiedTable ->
  [ColumnInfo ('Postgres pgKind)] ->
  Maybe Int ->
  MutationCTE ->
  MutationOutput ('Postgres pgKind) ->
  Options.StringifyNumbers ->
  Maybe NamingCase ->
  -- | Prepared params
  [PG.PrepArg] ->
  m EncJSON
executeMutationOutputQuery userInfo qt allCols preCalAffRows cte mutOutput strfyNum tCase prepArgs = do
  queryTags <- ask
  let queryTx :: (PG.FromRes a) => m a
      queryTx = do
        selectWith <- mkMutationOutputExp userInfo qt allCols preCalAffRows cte mutOutput strfyNum tCase
        let query = toQuery selectWith
            queryWithQueryTags = query {PG.getQueryText = (PG.getQueryText query) <> (_unQueryTagsComment queryTags)}
        -- See Note [Prepared statements in Mutations]
        liftTx (PG.rawQE dmlTxErrorHandler queryWithQueryTags prepArgs False)

  if checkPermissionRequired cte
    then withCheckPermission $ PG.getRow <$> queryTx
    else runIdentity . PG.getRow <$> queryTx

mutateAndFetchCols ::
  forall pgKind.
  (Backend ('Postgres pgKind), PostgresTranslateSelect pgKind) =>
  UserInfo ->
  QualifiedTable ->
  [ColumnInfo ('Postgres pgKind)] ->
  (MutationCTE, DS.Seq PG.PrepArg) ->
  Options.StringifyNumbers ->
  Maybe NamingCase ->
  PG.TxE QErr (MutateResp ('Postgres pgKind) TxtEncodedVal)
mutateAndFetchCols userInfo qt cols (cte, p) strfyNum tCase = do
  (colSel, customSQLCTEs) <-
    runWriterT
      $ S.SESelect
      <$> mkSQLSelect
        userInfo
        JASMultipleRows
        ( AnnSelectG selFlds tabFrom tabPerm noSelectArgs strfyNum tCase
        )
  let selectWith =
        S.SelectWith
          ( [(rawAlias, getMutationCTE cte)]
              <> customSQLToTopLevelCTEs customSQLCTEs
          )
          $ S.mkSelect
            { S.selExtr =
                S.Extractor
                  ( S.applyJsonBuildObj
                      [ S.SELit "affected_rows",
                        ( S.SESelect
                            $ S.mkSelect
                              { S.selExtr = [S.Extractor S.countStar Nothing],
                                S.selFrom = Just $ S.FromExp [S.FIIdentifier rawIdentifier]
                              }
                        ),
                        S.SELit "returning_columns",
                        colSel
                      ]
                  )
                  Nothing
                  : bool [] [S.Extractor (mkCheckErrorExp rawIdentifier) Nothing] (checkPermissionRequired cte)
            }
  let sqlText = toQuery selectWith
  let mutationTx :: (PG.FromRes a) => PG.TxE QErr a
      mutationTx =
        -- See Note [Prepared statements in Mutations]
        PG.rawQE dmlTxErrorHandler sqlText (toList p) False

  if checkPermissionRequired cte
    then withCheckPermission $ (first PG.getViaJSON . PG.getRow) <$> mutationTx
    else (PG.getViaJSON . runIdentity . PG.getRow) <$> mutationTx
  where
    rawAlias = S.mkTableAlias $ "mutres__" <> qualifiedObjectToText qt
    rawIdentifier = S.tableAliasToIdentifier rawAlias
    tabFrom = FromIdentifier $ FIIdentifier (unTableIdentifier rawIdentifier)
    tabPerm = TablePerm annBoolExpTrue Nothing
    selFlds = flip map cols
      $ \ci -> (fromCol @('Postgres pgKind) $ ciColumn ci, mkAnnColumnFieldAsText ci)

-------------- Validating insert input using external HTTP webhook -----------------------
type ValidateInputPayloadVersion = Int

validateInputPayloadVersion :: ValidateInputPayloadVersion
validateInputPayloadVersion = 1

newtype ValidateInputErrorResponse = ValidateInputErrorResponse {_vierMessage :: Text}
  deriving (Show, Eq)

$(J.deriveJSON hasuraJSON ''ValidateInputErrorResponse)

data HttpHandlerLog = HttpHandlerLog
  { _hhlUrl :: Text,
    _hhlRequest :: J.Value,
    _hhlRequestHeaders :: [HeaderConf],
    _hhlResponse :: J.Value,
    _hhlResponseStatus :: Int
  }
  deriving (Show)

$(J.deriveToJSON hasuraJSON ''HttpHandlerLog)

data ValidateInsertInputLog
  = VIILHttpHandler HttpHandlerLog

instance J.ToJSON ValidateInsertInputLog where
  toJSON (VIILHttpHandler httpHandlerLog) =
    J.object $ ["type" J..= ("http" :: String), "details" J..= J.toJSON httpHandlerLog]

instance L.ToEngineLog ValidateInsertInputLog L.Hasura where
  toEngineLog ahl = (L.LevelInfo, L.ELTValidateInputLog, J.toJSON ahl)

-- | Map of table name and the value that is being inserted for that table
-- This map is helpful for collecting all the insert mutation arguments for the
-- nested tables and then sending them all at onve to the input validation webhook.
type InsertValidationPayloadMap pgKind = InsOrdHashMap.InsOrdHashMap (TableName ('Postgres pgKind)) ([IR.AnnotatedInsertRow ('Postgres pgKind) (IR.UnpreparedValue ('Postgres pgKind))], (ValidateInput ResolvedWebhook))

validateUpdateMutation ::
  forall pgKind m.
  (MonadError QErr m, MonadIO m, Tracing.MonadTrace m) =>
  Env.Environment ->
  HTTP.Manager ->
  L.Logger L.Hasura ->
  UserInfo ->
  ResolvedWebhook ->
  [HeaderConf] ->
  Timeout ->
  Bool ->
  [HTTP.Header] ->
  IR.AnnotatedUpdateG ('Postgres pgKind) Void (IR.UnpreparedValue ('Postgres pgKind)) ->
  Maybe (HashMap G.Name (G.Value G.Variable)) ->
  m ()
validateUpdateMutation env manager logger userInfo resolvedWebHook confHeaders timeout forwardClientHeaders reqHeaders updateOperation maybeSelSetArgs = do
  inputData <-
    case maybeSelSetArgs of
      Just arguments -> do
        case (IR._auUpdateVariant updateOperation) of
          -- Mutation arguments for single update (eg: update_customer) are
          -- present as seperate root fields of the selection set.
          -- eg:
          Postgres.SingleBatch _ -> do
            -- this constructs something like: {"_set":{"name": {"_eq": "abc"}}, "where":{"id":{"_eq":10}}}
            let singleBatchinputVal =
                  J.object
                    $ map
                      (\(k, v) -> J.fromText (G.unName k) J..= graphQLToJSON v)
                      (HashMap.toList $ arguments)
            return (J.object ["input" J..= [singleBatchinputVal]])
          -- Mutation arguments for multiple updates (eg:
          -- update_customer_many) are present in the "updates" field of the
          -- selection set.
          -- Look for "updates" field and get the mutation arguments from it.
          -- eg: {"updates": [{"_set":{"id":{"_eq":10}}, "where":{"name":{"_eq":"abc"}}}]}
          Postgres.MultipleBatches _ -> do
            case (HashMap.lookup $$(G.litName "updates") arguments) of
              Nothing -> return $ J.Null
              Just val -> (return $ J.object ["input" J..= graphQLToJSON val])
      Nothing -> return J.Null
  validateMutation env manager logger userInfo resolvedWebHook confHeaders timeout forwardClientHeaders reqHeaders inputData

validateDeleteMutation ::
  forall m pgKind.
  (MonadError QErr m, MonadIO m, Tracing.MonadTrace m) =>
  Env.Environment ->
  HTTP.Manager ->
  L.Logger L.Hasura ->
  UserInfo ->
  ResolvedWebhook ->
  [HeaderConf] ->
  Timeout ->
  Bool ->
  [HTTP.Header] ->
  IR.AnnDelG ('Postgres pgKind) Void (IR.UnpreparedValue ('Postgres pgKind)) ->
  Maybe (HashMap G.Name (G.Value G.Variable)) ->
  m ()
validateDeleteMutation env manager logger userInfo resolvedWebHook confHeaders timeout forwardClientHeaders reqHeaders deleteOperation maybeSelSetArgs = do
  inputData <-
    case maybeSelSetArgs of
      Just arguments -> do
        -- this constructs something like: {"where":{"id":{"_eq":10}}}
        let deleteInputVal =
              J.object
                $ map
                  (\(k, v) -> J.fromText (G.unName k) J..= graphQLToJSON v)
                  (HashMap.toList $ arguments)
        if (_adIsDeleteByPk deleteOperation)
          then -- If the delete operation is delete_<table>_by_pk, then we need to
          -- include the pk_columns field manually in the input payload. This
          -- is needed, because unlike the update mutation, the pk_columns for
          -- `delete_<table>_by_pk` is not present in the mutation arguments.
          -- for eg: the `delete_<table>_by_pk` looks like:
          --
          -- mutation DeleteCustomerByPk {
          --   delete_customer_by_pk(id: 1) {
          --     id
          --    }
          -- }
          do
            let deleteInputValByPk = J.object ["pk_columns" J..= deleteInputVal]
            return (J.object ["input" J..= [deleteInputValByPk]])
          else return (J.object ["input" J..= [deleteInputVal]])
      Nothing -> return J.Null
  validateMutation env manager logger userInfo resolvedWebHook confHeaders timeout forwardClientHeaders reqHeaders inputData

validateMutation ::
  forall m.
  ( MonadError QErr m,
    MonadIO m,
    Tracing.MonadTrace m
  ) =>
  Env.Environment ->
  HTTP.Manager ->
  L.Logger L.Hasura ->
  UserInfo ->
  ResolvedWebhook ->
  [HeaderConf] ->
  Timeout ->
  Bool ->
  [HTTP.Header] ->
  J.Value ->
  m ()
validateMutation env manager logger userInfo (ResolvedWebhook urlText) confHeaders timeout forwardClientHeaders reqHeaders inputData = do
  let requestBody =
        J.object
          [ "version" J..= validateInputPayloadVersion,
            "session_variables" J..= _uiSession userInfo,
            "role" J..= _uiRole userInfo,
            "data" J..= inputData
          ]
  resolvedConfHeaders <- makeHeadersFromConf env confHeaders
  let clientHeaders = if forwardClientHeaders then mkClientHeadersForward reqHeaders else mempty
      -- Using HashMap to avoid duplicate headers between configuration headers
      -- and client headers where configuration headers are preferred
      hdrs = (HashMap.toList . HashMap.fromList) (resolvedConfHeaders <> defaultHeaders <> clientHeaders)
  initRequest <- liftIO $ HTTP.mkRequestThrow urlText
  let request =
        initRequest
          & Lens.set HTTP.method "POST"
          & Lens.set HTTP.headers hdrs
          & Lens.set HTTP.body (HTTP.RequestBodyLBS $ J.encode requestBody)
          & Lens.set HTTP.timeout (HTTP.responseTimeoutMicro (unTimeout timeout * 1000000)) -- (default: 10 seconds)
  httpResponse <-
    Tracing.traceHTTPRequest b3TraceContextPropagator request $ \request' ->
      liftIO . try $ HTTP.httpLbs request' manager

  case httpResponse of
    Left e ->
      throw500WithDetail "http exception when validating input data"
        $ J.toJSON
        $ HttpException e
    Right response -> do
      let responseStatus = response Lens.^. Wreq.responseStatus
          responseBody = response Lens.^. Wreq.responseBody
          responseBodyForLogging = fromMaybe (J.String $ lbsToTxt responseBody) $ J.decode' responseBody
      -- Log the details of the HTTP webhook call
      L.unLoggerTracing logger $ VIILHttpHandler $ HttpHandlerLog urlText requestBody confHeaders responseBodyForLogging (HTTP.statusCode responseStatus)
      if
        | HTTP.statusIsSuccessful responseStatus -> pure ()
        | responseStatus == HTTP.status400 -> do
            ValidateInputErrorResponse errorMessage <-
              J.eitherDecode responseBody `onLeft` \e ->
                throw500WithDetail "received invalid response from input validation webhook"
                  $ J.toJSON
                  $ "invalid response: "
                  <> e
            throw400 ValidationFailed errorMessage
        | otherwise -> do
            let err =
                  J.toJSON
                    $ "expecting 200 or 400 status code, but found "
                    ++ show (HTTP.statusCode responseStatus)
            throw500WithDetail "internal error when validating input data" err
