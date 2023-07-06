{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- = Reasonably efficient PostgreSQL live queries
--
-- The module implements /query multiplexing/, which is our implementation strategy for live queries
-- (i.e. GraphQL subscriptions) made against Postgres. Fundamentally, our implementation is built
-- around polling, which is never ideal, but it’s a lot easier to implement than trying to do something
-- event-based. To minimize the resource cost of polling, we use /multiplexing/, which is essentially
-- a two-tier batching strategy.
--
-- == The high-level idea
--
-- The objective is to minimize the number of concurrent polling workers to reduce database load as
-- much as possible. A very naïve strategy would be to group identical queries together so we only have
-- one poller per /unique/ active subscription. That’s a good start, but of course, in practice, most
-- queries differ slightly. However, it happens that they very frequently /only differ in their
-- variables/ (that is, GraphQL query variables and session variables), and in those cases, we try to
-- generated parameterized SQL. This means that the same prepared SQL query can be reused, just with a
-- different set of variables.
--
-- To give a concrete example, consider the following query:
--
-- > subscription vote_count($post_id: Int!) {
-- >   vote_count(where: {post_id: {_eq: $post_id}}) {
-- >     votes
-- >   }
-- > }
--
-- No matter what the client provides for @$post_id@, we will always generate the same SQL:
--
-- > SELECT votes FROM vote_count WHERE post_id = $1
--
-- If multiple clients subscribe to @vote_count@, we can certainly reuse the same prepared query. For
-- example, imagine we had 10 concurrent subscribers, each listening on a distinct @$post_id@:
--
-- > let postIds = [3, 11, 32, 56, 13, 97, 24, 43, 109, 48]
--
-- We could iterate over @postIds@ in Haskell, executing the same prepared query 10 times:
--
-- > for postIds $ \postId ->
-- >   PG.withQE defaultTxErrorHandler preparedQuery (Identity postId) True
--
-- Sadly, that on its own isn’t good enough. The overhead of running each query is large enough that
-- Postgres becomes overwhelmed if we have to serve lots of concurrent subscribers. Therefore, what we
-- want to be able to do is somehow make one query instead of ten.
--
-- === Multiplexing
--
-- This is where multiplexing comes in. By taking advantage of Postgres
-- <https://www.postgresql.org/docs/11/queries-table-expressions.html#QUERIES-LATERAL lateral joins>,
-- we can do the iteration in Postgres rather than in Haskell, allowing us to pay the query overhead
-- just once for all ten subscribers. Essentially, lateral joins add 'map'-like functionality to SQL,
-- so we can run our query once per @$post_id@:
--
-- > SELECT results.votes
-- > FROM unnest($1::integer[]) query_variables (post_id)
-- > LEFT JOIN LATERAL (
-- >   SELECT coalesce(json_agg(votes), '[]')
-- >   FROM vote_count WHERE vote_count.post_id = query_variables.post_id
-- > ) results ON true
--
-- If we generalize this approach just a little bit more, we can apply this transformation to arbitrary
-- queries parameterized over arbitrary session and query variables!
--
-- == Implementation overview
--
-- To support query multiplexing, we maintain a tree of the following types, where @>@ should be read
-- as “contains”:
--
-- @
-- 'SubscriptionsState' > 'Poller' > 'Cohort' > 'Subscriber'
-- @
--
-- Here’s a brief summary of each type’s role:
--
--   * A 'Subscriber' is an actual client with an open websocket connection.
--
--   * A 'Cohort' is a set of 'Subscriber's that are all subscribed to the same query /with the exact
--     same variables/. (By batching these together, we can do better than multiplexing, since we can
--     just query the data once.)
--
--   * A 'Poller' is a worker thread for a single, multiplexed query. It fetches data for a set of
--     'Cohort's that all use the same parameterized query, but have different sets of variables.
--
--   * Finally, the 'SubscriptionsState' is the top-level container that holds all the active 'Poller's.
--
-- Additional details are provided by the documentation for individual bindings.
module Hasura.GraphQL.Execute.Subscription.Plan
  ( CohortId,
    dummyCohortId,
    newCohortId,
    CohortIdArray (..),
    CohortVariablesArray (..),
    CohortVariables,
    _cvCursorVariables,
    mkCohortVariables,
    ValidatedVariables (..),
    mkUnsafeValidateVariables,
    modifyCursorCohortVariables,
    ValidatedQueryVariables,
    ValidatedSyntheticVariables,
    ValidatedCursorVariables,
    SubscriptionQueryPlan (..),
    SubscriptionQueryPlanExplanation (..),
    ParameterizedSubscriptionQueryPlan (..),
    CursorVariableValues (..),
    cvSessionVariables,
    cvCursorVariables,
    cvQueryVariables,
    cvSyntheticVariables,
    unValidatedVariables,
    applyModifier,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson.Extended qualified as J
import Data.Aeson.Ordered qualified as JO
import Data.Aeson.TH qualified as J
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as Set
import Data.Monoid (Endo (..))
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Database.PG.Query qualified as PG
import Database.PG.Query.PTI qualified as PTI
import Hasura.Backends.Postgres.SQL.Value
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.Session (SessionVariable, SessionVariables, filterSessionVariables)
import Language.GraphQL.Draft.Syntax qualified as G
import PostgreSQL.Binary.Encoding qualified as PE

----------------------------------------------------------------------------------------------------
-- Variable validation

-- | When running multiplexed queries, we have to be especially careful about user
-- input, since invalid values will cause the query to fail, causing collateral
-- damage for anyone else multiplexed into the same query.  Therefore, we
-- pre-validate variables against Postgres by executing a no-op query of the shape
--
-- > SELECT 'v1'::t1, 'v2'::t2, ..., 'vn'::tn
--
-- so if any variable values are invalid, the error will be caught early.
newtype ValidatedVariables f = ValidatedVariables {_unValidatedVariables :: (f TxtEncodedVal)}

deriving instance (Show (f TxtEncodedVal)) => Show (ValidatedVariables f)

deriving instance (Eq (f TxtEncodedVal)) => Eq (ValidatedVariables f)

deriving instance (Hashable (f TxtEncodedVal)) => Hashable (ValidatedVariables f)

deriving instance (J.ToJSON (f TxtEncodedVal)) => J.ToJSON (ValidatedVariables f)

deriving instance (Semigroup (f TxtEncodedVal)) => Semigroup (ValidatedVariables f)

deriving instance (Monoid (f TxtEncodedVal)) => Monoid (ValidatedVariables f)

$(makeLenses 'ValidatedVariables)

type ValidatedQueryVariables = ValidatedVariables (HashMap.HashMap G.Name)

type ValidatedSyntheticVariables = ValidatedVariables []

type ValidatedCursorVariables = ValidatedVariables (HashMap.HashMap G.Name)

mkUnsafeValidateVariables :: f TxtEncodedVal -> ValidatedVariables f
mkUnsafeValidateVariables = ValidatedVariables

----------------------------------------------------------------------------------------------------
-- Cohort

newtype CohortId = CohortId {unCohortId :: UUID}
  deriving (Show, Eq, Hashable, J.ToJSON, J.FromJSON, PG.FromCol)

newCohortId :: (MonadIO m) => m CohortId
newCohortId = CohortId <$> liftIO UUID.nextRandom

dummyCohortId :: CohortId
dummyCohortId = CohortId UUID.nil

data CohortVariables = CohortVariables
  { _cvSessionVariables :: !SessionVariables,
    _cvQueryVariables :: !ValidatedQueryVariables,
    -- | To allow more queries to be multiplexed together, we introduce “synthetic”
    -- variables for /all/ SQL literals in a query, even if they don’t correspond to
    -- any GraphQL variable. For example, the query
    --
    -- > subscription latest_tracks($condition: tracks_bool_exp!) {
    -- >   tracks(where: $tracks_bool_exp) {
    -- >     id
    -- >     title
    -- >   }
    -- > }
    --
    -- might be executed with similar values for @$condition@, such as @{"album_id":
    -- {"_eq": "1"}}@ and @{"album_id": {"_eq": "2"}}@.
    --
    -- Normally, we wouldn’t bother parameterizing over the @1@ and @2@ literals in the
    -- resulting query because we can’t cache that query plan (since different
    -- @$condition@ values could lead to different SQL). However, for live queries, we
    -- can still take advantage of the similarity between the two queries by
    -- multiplexing them together, so we replace them with references to synthetic
    -- variables.
    _cvSyntheticVariables :: !ValidatedSyntheticVariables,
    -- | Cursor variables contain the latest value of the cursor.
    --   The value of the cursor variables are updated after every poll.
    --   If the value has been changed - see [Streaming subscription polling].
    --   Cursor variables are only used in the case of streaming subscriptions,
    --   for live queries it will be empty.
    _cvCursorVariables :: !ValidatedCursorVariables
  }
  deriving (Show, Eq, Generic)

instance Hashable CohortVariables

$(makeLenses 'CohortVariables)

modifyCursorCohortVariables ::
  ValidatedCursorVariables ->
  CohortVariables ->
  CohortVariables
modifyCursorCohortVariables validatedCursorVariables cohortVariables =
  cohortVariables {_cvCursorVariables = validatedCursorVariables}

-- | Builds a cohort's variables by only using the session variables that
-- are required for the subscription
mkCohortVariables ::
  Set.HashSet SessionVariable ->
  SessionVariables ->
  ValidatedQueryVariables ->
  ValidatedSyntheticVariables ->
  ValidatedCursorVariables ->
  CohortVariables
mkCohortVariables requiredSessionVariables sessionVariableValues =
  CohortVariables
    $ filterSessionVariables
      (\k _ -> Set.member k requiredSessionVariables)
      sessionVariableValues

instance J.ToJSON CohortVariables where
  toJSON (CohortVariables sessionVars queryVars syntheticVars cursorVars) =
    J.object
      [ "session" J..= sessionVars,
        "query" J..= queryVars,
        "synthetic" J..= syntheticVars,
        "cursor" J..= cursorVars
      ]

-- These types exist only to use the Postgres array encoding.
newtype CohortIdArray = CohortIdArray {unCohortIdArray :: [CohortId]}
  deriving (Show, Eq)

instance PG.ToPrepArg CohortIdArray where
  toPrepVal (CohortIdArray l) = PG.toPrepValHelper PTI.unknown encoder $ map unCohortId l
    where
      encoder = PE.array (PTI.unOid PTI.uuid) . PE.dimensionArray foldl' (PE.encodingArray . PE.uuid)

newtype CohortVariablesArray = CohortVariablesArray {unCohortVariablesArray :: [CohortVariables]}
  deriving (Show, Eq)

-- to get around a CockroachDB JSON issue, we encode as JSONB instead of JSON
-- here. The query already casts it to the JSON it expects.
-- we can change this back to sending JSON once this issue is fixed:
-- https://github.com/cockroachdb/cockroach/issues/90839
instance PG.ToPrepArg CohortVariablesArray where
  toPrepVal (CohortVariablesArray l) =
    PG.toPrepValHelper PTI.jsonb_array encoder (map J.toJSON l)
    where
      encoder = PE.array (PTI.unOid PTI.jsonb) . PE.dimensionArray foldl' (PE.encodingArray . PE.jsonb_ast)

applyModifier :: (Maybe (Endo JO.Value)) -> BS.ByteString -> BS.ByteString
applyModifier Nothing = id
applyModifier (Just modifier) = \bs -> case JO.decode bs of
  Nothing -> bs
  Just v -> encJToBS . encJFromOrderedValue $ appEndo modifier v

----------------------------------------------------------------------------------------------------
-- Live query plans

-- | A self-contained, ready-to-execute subscription plan. Contains enough information
-- to find an existing poller that this can be added to /or/ to create a new poller
-- if necessary.
data SubscriptionQueryPlan (b :: BackendType) q = SubscriptionQueryPlan
  { _sqpParameterizedPlan :: ParameterizedSubscriptionQueryPlan b q,
    _sqpSourceConfig :: SourceConfig b,
    _sqpCohortId :: CohortId,
    _sqpResolvedConnectionTemplate :: ResolvedConnectionTemplate b,
    _sqpVariables :: CohortVariables,
    -- | We need to know if the source has a namespace so that we can wrap it around
    -- the response from the DB
    _sqpNamespace :: Maybe G.Name
  }

data ParameterizedSubscriptionQueryPlan (b :: BackendType) q = ParameterizedSubscriptionQueryPlan
  { _plqpRole :: !RoleName,
    _plqpQuery :: !q
  }
  deriving (Show)

$(J.deriveToJSON hasuraJSON ''ParameterizedSubscriptionQueryPlan)

data SubscriptionQueryPlanExplanation = SubscriptionQueryPlanExplanation
  { _sqpeSql :: !Text,
    _sqpePlan :: ![Text],
    _sqpeVariables :: !CohortVariables
  }
  deriving (Show)

$(J.deriveToJSON hasuraJSON ''SubscriptionQueryPlanExplanation)

--------------------------------------------------------------------------
--- Streaming Subscriptions

newtype CursorVariableValues = CursorVariableValues (HashMap G.Name TxtEncodedVal)
  deriving (J.FromJSON, J.ToJSON, Eq, Show)
