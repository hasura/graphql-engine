{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MySQL.Instances.Execute where

import Data.Aeson qualified as J
import Data.String (IsString (..))
import Hasura.Backends.MySQL.Connection (runJSONPathQuery)
import Hasura.Backends.MySQL.Plan (planQuery)
import Hasura.Backends.MySQL.ToQuery (Printer, fromSelect, toQueryFlat, toQueryPretty)
import Hasura.Backends.MySQL.Types qualified as MySQL
import Hasura.Base.Error (QErr, throw500)
import Hasura.EncJSON (encJFromText)
import Hasura.GraphQL.Execute.Backend
  ( BackendExecute (..),
    DBStepInfo (..),
    PreparedQuery,
  )
import Hasura.GraphQL.Parser (UnpreparedValue)
import Hasura.Prelude
import Hasura.RQL.IR (QueryDB, SourceRelationshipSelection)
import Hasura.RQL.Types (BackendType (MySQL), SourceConfig, SourceName)
import Hasura.RQL.Types qualified as RQLTypes
import Hasura.Session (UserInfo (..))

instance BackendExecute 'MySQL where
  type PreparedQuery 'MySQL = Text
  type MultiplexedQuery 'MySQL = Void
  type ExecutionMonad 'MySQL = ExceptT QErr IO
  mkDBQueryPlan = mysqlDBQueryPlan
  mkDBMutationPlan = error "mkDBMutationPlan: MySQL backend does not support this operation yet."
  mkDBSubscriptionPlan _ _ _ _ = error "mkDBSubscriptionPlan: MySQL backend does not support this operation yet."
  mkDBQueryExplain = error "mkDBQueryExplain: MySQL backend does not support this operation yet."
  mkLiveQueryExplain _ = error "mkLiveQueryExplain: MySQL backend does not support this operation yet."

  -- NOTE: Currently unimplemented!.
  --
  -- This function is just a stub for future implementation; for now it just
  -- throws a 500 error.
  mkDBRemoteRelationshipPlan =
    mysqlDBRemoteRelationshipPlan

mysqlDBQueryPlan ::
  forall m.
  ( MonadError QErr m
  ) =>
  UserInfo ->
  RQLTypes.SourceName ->
  RQLTypes.SourceConfig 'MySQL ->
  QueryDB 'MySQL (Const Void) (UnpreparedValue 'MySQL) ->
  m (DBStepInfo 'MySQL)
mysqlDBQueryPlan userInfo sourceName sourceConfig qrf = do
  let sessionVariables = _uiSession userInfo
  statement :: MySQL.Select <- planQuery sessionVariables qrf
  let printer :: Printer = fromSelect statement
      queryString = toQueryPretty printer
      pool = MySQL.scConnectionPool sourceConfig
      mysqlQuery = encJFromText <$> runJSONPathQuery pool (toQueryFlat printer)
  pure $ DBStepInfo @'MySQL sourceName sourceConfig (Just $ fromString $ show queryString) mysqlQuery

--------------------------------------------------------------------------------
-- Remote Relationships (e.g. DB-to-DB Joins, remote schema joins, etc.)
--------------------------------------------------------------------------------

-- | Construct an action (i.e. 'DBStepInfo') which can marshal some remote
-- relationship information into a form that MySQL can query against.
--
-- XXX: Currently unimplemented; the Postgres implementation uses
-- @jsonb_to_recordset@ to query the remote relationship, however this
-- functionality doesn't exist in MYSQL.
--
-- NOTE: The following typeclass constraints will be necessary when implementing
-- this function for real:
--
-- @
--   MonadQueryTags m
--   Backend 'MySQL
-- @
mysqlDBRemoteRelationshipPlan ::
  forall m.
  ( MonadError QErr m
  ) =>
  UserInfo ->
  SourceName ->
  SourceConfig 'MySQL ->
  -- | List of json objects, each of which becomes a row of the table.
  NonEmpty J.Object ->
  -- | The above objects have this schema
  --
  -- XXX: What is this for/what does this mean?
  HashMap RQLTypes.FieldName (RQLTypes.Column 'MySQL, RQLTypes.ScalarType 'MySQL) ->
  -- | This is a field name from the lhs that *has* to be selected in the
  -- response along with the relationship.
  RQLTypes.FieldName ->
  (RQLTypes.FieldName, SourceRelationshipSelection 'MySQL (Const Void) UnpreparedValue) ->
  m (DBStepInfo 'MySQL)
mysqlDBRemoteRelationshipPlan _userInfo _sourceName _sourceConfig _lhs _lhsSchema _argumentId _relationship = do
  throw500 "mkDBRemoteRelationshipPlan: MySQL does not currently support generalized joins."
