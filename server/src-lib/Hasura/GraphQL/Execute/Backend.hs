module Hasura.GraphQL.Execute.Backend
  ( BackendExecute (..),
    DBStepInfo (..),
    ActionResult (..),
    withNoStatistics,
    ExecutionPlan,
    ExecutionStep (..),
    ExplainPlan (..),
    OnBaseMonad (..),
    convertRemoteSourceRelationship,
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Aeson.Ordered qualified as JO
import Data.Environment qualified as Env
import Data.Kind (Type)
import Data.Text.Extended
import Data.Text.NonEmpty (mkNonEmptyTextUnsafe)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Action.Types (ActionExecutionPlan)
import Hasura.GraphQL.Execute.RemoteJoin.Types
import Hasura.GraphQL.Execute.Subscription.Plan
import Hasura.GraphQL.Namespace (RootFieldAlias, RootFieldMap)
import Hasura.GraphQL.Parser.Variable qualified as G
import Hasura.GraphQL.Transport.HTTP.Protocol qualified as GH
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.IR
import Hasura.RQL.IR.ModelInformation
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column (ColumnType, fromCol)
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Relationships.Local (Nullable (..))
import Hasura.RQL.Types.ResultCustomization
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RemoteSchema.SchemaCache
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Types
import Hasura.Session
import Hasura.Tracing (MonadTrace)
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP

-- | This typeclass enacapsulates how a given backend translates a root field into an execution
-- plan. For now, each root field maps to one execution step, but in the future, when we have
-- a client-side dataloader, each root field might translate into a multi-step plan.
class
  ( Backend b,
    ToTxt (MultiplexedQuery b),
    Show (ResolvedConnectionTemplate b),
    Eq (ResolvedConnectionTemplate b),
    Hashable (ResolvedConnectionTemplate b)
  ) =>
  BackendExecute (b :: BackendType)
  where
  -- generated query information
  type PreparedQuery b :: Type
  type MultiplexedQuery b :: Type
  type ExecutionMonad b :: (Type -> Type) -> (Type -> Type)

  -- execution plan generation
  mkDBQueryPlan ::
    forall m.
    ( MonadError QErr m,
      MonadQueryTags m,
      MonadReader QueryTagsComment m,
      MonadIO m,
      MonadGetPolicies m
    ) =>
    UserInfo ->
    SourceName ->
    SourceConfig b ->
    QueryDB b Void (UnpreparedValue b) ->
    [HTTP.Header] ->
    Maybe G.Name ->
    m ((DBStepInfo b), [ModelInfoPart])
  mkDBMutationPlan ::
    forall m.
    ( MonadError QErr m,
      MonadIO m,
      MonadQueryTags m,
      MonadReader QueryTagsComment m,
      Tracing.MonadTrace m
    ) =>
    Env.Environment ->
    HTTP.Manager ->
    L.Logger L.Hasura ->
    UserInfo ->
    Options.StringifyNumbers ->
    SourceName ->
    SourceConfig b ->
    MutationDB b Void (UnpreparedValue b) ->
    [HTTP.Header] ->
    Maybe G.Name ->
    Maybe (HashMap G.Name (G.Value G.Variable)) ->
    m (DBStepInfo b, [ModelInfoPart])
  mkLiveQuerySubscriptionPlan ::
    forall m.
    ( MonadError QErr m,
      MonadIO m,
      MonadBaseControl IO m,
      MonadReader QueryTagsComment m
    ) =>
    UserInfo ->
    SourceName ->
    SourceConfig b ->
    Maybe G.Name ->
    RootFieldMap (QueryDB b Void (UnpreparedValue b)) ->
    [HTTP.Header] ->
    Maybe G.Name ->
    m (SubscriptionQueryPlan b (MultiplexedQuery b), [ModelInfoPart])
  mkDBStreamingSubscriptionPlan ::
    forall m.
    ( MonadError QErr m,
      MonadIO m,
      MonadBaseControl IO m,
      MonadReader QueryTagsComment m
    ) =>
    UserInfo ->
    SourceName ->
    SourceConfig b ->
    (RootFieldAlias, (QueryDB b Void (UnpreparedValue b))) ->
    [HTTP.Header] ->
    Maybe G.Name ->
    m (SubscriptionQueryPlan b (MultiplexedQuery b), [ModelInfoPart])
  mkDBQueryExplain ::
    forall m.
    ( MonadError QErr m,
      MonadIO m
    ) =>
    RootFieldAlias ->
    UserInfo ->
    SourceName ->
    SourceConfig b ->
    QueryDB b Void (UnpreparedValue b) ->
    [HTTP.Header] ->
    Maybe G.Name ->
    m (AB.AnyBackend (DBStepInfo))
  mkSubscriptionExplain ::
    ( MonadError QErr m,
      MonadIO m,
      MonadBaseControl IO m
    ) =>
    SubscriptionQueryPlan b (MultiplexedQuery b) ->
    m SubscriptionQueryPlanExplanation

  mkDBRemoteRelationshipPlan ::
    forall m.
    ( MonadError QErr m,
      MonadQueryTags m,
      MonadIO m,
      MonadGetPolicies m
    ) =>
    UserInfo ->
    SourceName ->
    SourceConfig b ->
    -- | List of json objects, each of which becomes a row of the table.
    NonEmpty J.Object ->
    -- | The above objects have this schema.
    HashMap FieldName (Column b, ScalarType b) ->
    -- | This is a field name from the lhs that *has* to be selected in the
    -- response along with the relationship. It is populated in
    -- `Hasura.GraphQL.Execute.RemoteJoin.Join.processRemoteJoins_` and
    -- the function `convertRemoteSourceRelationship` below assumes it
    -- to be returned as either a number or a string with a number in it
    FieldName ->
    (FieldName, SourceRelationshipSelection b Void UnpreparedValue) ->
    [HTTP.Header] ->
    Maybe G.Name ->
    Options.StringifyNumbers ->
    m (DBStepInfo b, [ModelInfoPart])

-- | This is a helper function to convert a remote source's relationship to a
-- normal relationship to a temporary table. This function can be used to
-- implement executeRemoteRelationship function in databases which support
-- constructing a temporary table for a list of json objects.
convertRemoteSourceRelationship ::
  forall b.
  (Backend b) =>
  -- | Join columns for the relationship
  HashMap (ColumnPath b) (ColumnPath b) ->
  -- | The LHS of the join, this is the expression which selects from json
  -- objects
  SelectFromG b (UnpreparedValue b) ->
  -- | This is the __argument__ id column, that needs to be added to the response
  -- This is used by by the remote joins processing logic to convert the
  -- response from upstream to join indices
  Column b ->
  -- | This is the type of the __argument__ id column
  ColumnType b ->
  -- | The relationship column and its name (how it should be selected in the
  -- response)
  (FieldName, SourceRelationshipSelection b Void UnpreparedValue) ->
  Options.StringifyNumbers ->
  QueryDB b Void (UnpreparedValue b)
convertRemoteSourceRelationship
  columnMapping
  selectFrom
  argumentIdColumn
  argumentIdColumnType
  (relationshipName, relationship)
  stringifyNumbers =
    QDBMultipleRows simpleSelect
    where
      -- TODO: FieldName should have also been a wrapper around NonEmptyText
      relName = RelName $ mkNonEmptyTextUnsafe $ getFieldNameTxt relationshipName

      relationshipField = case relationship of
        SourceRelationshipObject s ->
          AFObjectRelation $ AnnRelationSelectG relName columnMapping Nullable s
        SourceRelationshipArray s ->
          AFArrayRelation $ ASSimple $ AnnRelationSelectG relName columnMapping Nullable s
        SourceRelationshipArrayAggregate s ->
          AFArrayRelation $ ASAggregate $ AnnRelationSelectG relName columnMapping Nullable s

      argumentIdField =
        ( fromCol @b argumentIdColumn,
          AFColumn
            $ AnnColumnField
              { _acfColumn = argumentIdColumn,
                _acfType = argumentIdColumnType,
                _acfAsText = False,
                _acfArguments = Nothing,
                _acfRedactionExpression = NoRedaction
              }
        )

      simpleSelect =
        AnnSelectG
          { _asnFields = [argumentIdField, (relationshipName, relationshipField)],
            _asnFrom = selectFrom,
            _asnPerm = TablePerm annBoolExpTrue Nothing,
            _asnArgs = noSelectArgs,
            _asnStrfyNum = stringifyNumbers,
            _asnNamingConvention = Nothing
          }

data DBStepInfo b = DBStepInfo
  { dbsiSourceName :: SourceName,
    dbsiSourceConfig :: SourceConfig b,
    dbsiPreparedQuery :: Maybe (PreparedQuery b),
    dbsiAction :: OnBaseMonad (ExecutionMonad b) (ActionResult b),
    dbsiResolvedConnectionTemplate :: ResolvedConnectionTemplate b
  }

data ActionResult b = ActionResult
  { arStatistics :: Maybe (ExecutionStatistics b),
    arResult :: EncJSON
  }

-- | Lift a result from the database into an 'ActionResult'.
withNoStatistics :: EncJSON -> ActionResult b
withNoStatistics arResult = ActionResult {arStatistics = Nothing, arResult}

-- | Provides an abstraction over the base monad in which a computation runs.
--
-- Given a transformer @t@ and a type @a@, @OnBaseMonad t a@ represents a
-- computation of type @t m a@, for any base monad @m@. This allows 'DBStepInfo'
-- to store a backend-specific computation, using a backend-specific monad
-- transformer, on top of the base app monad, without 'DBStepInfo' needing to
-- know about the base monad @m@.
--
-- However, this kind of type erasure forces us to bundle all of the constraints
-- on the base monad @m@ here. The constraints here are the union of the
-- constraints required across all backends. If it were possible to express
-- constraint functions of the form @(Type -> Type) -> Constraint@ at the type
-- level, we could make the list of constraints a type family in
-- 'BackendExecute', allowing each backend to specify its own specific
-- constraints; and we could then provide the list of constraints as an
-- additional argument to @OnBaseMonad@, pushing the requirement to implement
-- the union of all constraints to the base execution functions.
--
-- All backends require @MonadError QErr@ to report errors, and 'MonadIO' to be
-- able to communicate over the network. Most of them require 'MonadTrace' to
-- be able to create new spans as part of the execution, and several use
-- @MonadBaseControl IO@ to use 'try' in their error handling.
newtype OnBaseMonad t a = OnBaseMonad
  { runOnBaseMonad :: forall m. (Functor (t m), MonadIO m, MonadBaseControl IO m, MonadTrace m, MonadError QErr m) => t m a
  }

instance Functor (OnBaseMonad t) where
  fmap f (OnBaseMonad xs) = OnBaseMonad (fmap f xs)

-- | The result of an explain query: for a given root field (denoted by its name): the generated SQL
-- query, and the detailed explanation obtained from the database (if any). We mostly use this type
-- as an intermediary step, and immediately tranform any value we obtain into an equivalent JSON
-- representation.
data ExplainPlan = ExplainPlan
  { _fpField :: !RootFieldAlias,
    _fpSql :: !(Maybe Text),
    _fpPlan :: !(Maybe [Text])
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON ExplainPlan where
  toJSON = J.genericToJSON $ J.aesonPrefix J.camelCase

-- | One execution step to processing a GraphQL query (e.g. one root field).
data ExecutionStep where
  -- | A query to execute against the database
  ExecStepDB ::
    HTTP.ResponseHeaders ->
    AB.AnyBackend DBStepInfo ->
    Maybe RemoteJoins ->
    ExecutionStep
  -- | Execute an action
  ExecStepAction ::
    ActionExecutionPlan ->
    ActionsInfo ->
    Maybe RemoteJoins ->
    ExecutionStep
  -- | A graphql query to execute against a remote schema
  ExecStepRemote ::
    !RemoteSchemaInfo ->
    !ResultCustomizer ->
    !GH.GQLReqOutgoing ->
    Maybe RemoteJoins ->
    ExecutionStep
  -- | Output a plain JSON object
  ExecStepRaw ::
    JO.Value ->
    ExecutionStep
  ExecStepMulti ::
    [ExecutionStep] ->
    ExecutionStep

-- | The series of steps that need to be executed for a given query. For now, those steps are all
-- independent. In the future, when we implement a client-side dataloader and generalized joins,
-- this will need to be changed into an annotated tree.
type ExecutionPlan = RootFieldMap ExecutionStep
