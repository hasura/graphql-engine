-- |
-- Definitions for schema building configuration.
module Hasura.RQL.Types.Schema.Options
  ( SchemaOptions (..),
    StringifyNumbers (..),
    DangerouslyCollapseBooleans (..),
    BackwardsCompatibleNullInNonNullableVariables (..),
    RemoteNullForwardingPolicy (..),
    InferFunctionPermissions (..),
    RemoteSchemaPermissions (..),
    OptimizePermissionFilters (..),
    IncludeAggregationPredicates (..),
    IncludeStreamFields (..),
    IncludeUpdateManyFields (..),
    BigQueryStringNumericInput (..),
    IncludeGroupByAggregateFields (..),
    UsePostgresArrays (..),
    NoNullUnboundVariableDefault (..),
    RemoveEmptySubscriptionResponses (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withBool)
import Hasura.Prelude

-- | The record of options required to build the schema. This includes options
-- that can be enabled with 'Hasura.Server.Types.ExperimentalFeature' flags.
data SchemaOptions = SchemaOptions
  { soStringifyNumbers :: StringifyNumbers,
    soDangerousBooleanCollapse :: DangerouslyCollapseBooleans,
    soRemoteNullForwardingPolicy :: RemoteNullForwardingPolicy,
    soInferFunctionPermissions :: InferFunctionPermissions,
    soOptimizePermissionFilters :: OptimizePermissionFilters,
    soIncludeUpdateManyFields :: IncludeUpdateManyFields,
    soIncludeAggregationPredicates :: IncludeAggregationPredicates,
    soIncludeStreamFields :: IncludeStreamFields,
    soBigQueryStringNumericInput :: BigQueryStringNumericInput,
    soIncludeGroupByAggregateFields :: IncludeGroupByAggregateFields,
    soPostgresArrays :: UsePostgresArrays,
    soNoNullUnboundVariableDefault :: NoNullUnboundVariableDefault,
    soRemoveEmptySubscriptionResponses :: RemoveEmptySubscriptionResponses
  }

-- | Should we represent numbers in our responses as numbers, or strings?
-- Some backends can return numbers that exceed the bounds of JSON's own
-- number type. In these cases, we can use stringified versions of these
-- numbers to avoid this problem.
data StringifyNumbers
  = StringifyNumbers
  | Don'tStringifyNumbers
  deriving (Eq, Show)

-- | Should we include `TABLE_updates` fields in schemas
-- This is a toggle so that users can opt-in, and so that we can rename
-- any tables that this may conflict with if needed
data IncludeUpdateManyFields
  = IncludeUpdateManyFields
  | Don'tIncludeUpdateManyFields
  deriving (Eq, Show)

-- | Should we include `TABLE_stream` fields in schemas
-- This is a toggle so that users can opt-in, and so that we can rename
-- any tables that this may conflict with if needed
data IncludeStreamFields
  = IncludeStreamFields
  | Don'tIncludeStreamFields
  deriving (Eq, Show)

-- | Should we include aggregation functions in where clauses?
-- Because this has the potential to cause naming conflicts in graphql schema
-- types, this flag allows users to toggle the feature off if it an upgrade breaks
-- their setup.
data IncludeAggregationPredicates
  = IncludeAggregationPredicates
  | Don'tIncludeAggregationPredicates

-- | Should Boolean fields be collapsed to 'True' when a null value is
-- given? This was the behaviour of Hasura V1, and is now discouraged.
data DangerouslyCollapseBooleans
  = DangerouslyCollapseBooleans
  | Don'tDangerouslyCollapseBooleans
  deriving (Eq, Show)

instance FromJSON DangerouslyCollapseBooleans where
  parseJSON =
    withBool "DangerouslyCollapseBooleans"
      $ pure
      . \case
        True -> DangerouslyCollapseBooleans
        False -> Don'tDangerouslyCollapseBooleans

instance ToJSON DangerouslyCollapseBooleans where
  toJSON = \case
    DangerouslyCollapseBooleans -> Bool True
    Don'tDangerouslyCollapseBooleans -> Bool False

-- | Should `null` values allowed for variables whose type is declared
-- as non-nullable in a GraphQL query.
-- Until version 2.34.0 (except 2.11.11), `null` value is validated for
-- non-nullable variables. It is being fixed, to keep backwards compatible
-- this option is introduced.
data BackwardsCompatibleNullInNonNullableVariables
  = AllowNullInNonNullableVariables
  | Don'tAllowNullInNonNullableVariables
  deriving (Eq, Show)

instance FromJSON BackwardsCompatibleNullInNonNullableVariables where
  parseJSON =
    withBool "BackwardsCompatibleNullInNonNullableVariables"
      $ pure
      . bool Don'tAllowNullInNonNullableVariables AllowNullInNonNullableVariables

instance ToJSON BackwardsCompatibleNullInNonNullableVariables where
  toJSON = \case
    AllowNullInNonNullableVariables -> Bool True
    Don'tAllowNullInNonNullableVariables -> Bool False

data RemoteNullForwardingPolicy
  = RemoteForwardAccurately
  | RemoteOnlyForwardNonNull
  deriving (Show, Eq)

instance FromJSON RemoteNullForwardingPolicy where
  parseJSON =
    withBool "RemoteNullForwardingPolicy"
      $ pure
      . \case
        False -> RemoteForwardAccurately
        True -> RemoteOnlyForwardNonNull

instance ToJSON RemoteNullForwardingPolicy where
  toJSON = \case
    RemoteForwardAccurately -> Bool False
    RemoteOnlyForwardNonNull -> Bool True

-- | Should we infer function permissions? If this flag is set to
-- 'InferFunctionPermissions', we may fail to build expression parsers
-- in 'buildQueryAndSubscriptionFields' for users with unrecognised roles.
data InferFunctionPermissions
  = InferFunctionPermissions
  | Don'tInferFunctionPermissions
  deriving (Eq, Show)

instance FromJSON InferFunctionPermissions where
  parseJSON =
    withBool "InferFunctionPermissions"
      $ pure
      . \case
        True -> InferFunctionPermissions
        False -> Don'tInferFunctionPermissions

instance ToJSON InferFunctionPermissions where
  toJSON = \case
    InferFunctionPermissions -> Bool True
    Don'tInferFunctionPermissions -> Bool False

-- | Should we enable remote schema permissions? If so, these permissions will
-- influence 'Hasura.GraphQL.Schema.buildGQLContext'.
data RemoteSchemaPermissions
  = EnableRemoteSchemaPermissions
  | DisableRemoteSchemaPermissions
  deriving (Eq, Show)

instance FromJSON RemoteSchemaPermissions where
  parseJSON =
    withBool "RemoteSchemaPermissions"
      $ pure
      . \case
        True -> EnableRemoteSchemaPermissions
        False -> DisableRemoteSchemaPermissions

instance ToJSON RemoteSchemaPermissions where
  toJSON = \case
    EnableRemoteSchemaPermissions -> Bool True
    DisableRemoteSchemaPermissions -> Bool False

-- | Should we attempt to deduplicate permission filters? This flag is used in
-- 'Hasura.GraphQL.Schema.Select.relationshipField' to determine whether
-- certain row-level permission filters can be dropped in certain cases.
data OptimizePermissionFilters
  = OptimizePermissionFilters
  | Don'tOptimizePermissionFilters
  deriving (Eq, Show)

-- | Should we enable string-accepting scalar parsers for BigQuery sources
data BigQueryStringNumericInput
  = EnableBigQueryStringNumericInput
  | DisableBigQueryStringNumericInput
  deriving (Eq, Show)

data IncludeGroupByAggregateFields
  = IncludeGroupByAggregateFields
  | ExcludeGroupByAggregateFields
  deriving (Eq, Show)

-- | if we use Postgres arrays then an array of `text` becomes `[String!]`,
-- however we want users to have the option to make it output `_text` like it
-- did before for compatibility.
data UsePostgresArrays
  = UsePostgresArrays
  | DontUsePostgresArrays
  deriving (Eq, Show)

-- | The spec says that unbound nullable variables with no defaults should be
-- "removed" from the query. In other words, if no value for @$var@ is given,
-- and no default value exists within the query, then @{ foo: $var }@ should be
-- equivalent to @{}@. Without the flag, @$var@ becomes @null@.
data NoNullUnboundVariableDefault
  = DefaultUnboundNullableVariablesToNull
  | RemoveUnboundNullableVariablesFromTheQuery
  deriving (Eq, Show)

instance FromJSON NoNullUnboundVariableDefault where
  parseJSON =
    withBool "NoNullUnboundVariableDefault"
      $ pure
      . \case
        True -> RemoveUnboundNullableVariablesFromTheQuery
        False -> DefaultUnboundNullableVariablesToNull

instance ToJSON NoNullUnboundVariableDefault where
  toJSON = \case
    RemoveUnboundNullableVariablesFromTheQuery -> Bool True
    DefaultUnboundNullableVariablesToNull -> Bool False

-- | When we're dealing with many multiplexed streaming subscriptions that
-- don't update often, network overhead can become very large due to us
-- returning empty result sets from the database. This option allows the user
-- to stipulate that empty result sets should not be returned.
data RemoveEmptySubscriptionResponses
  = RemoveEmptyResponses
  | PreserveEmptyResponses
  deriving (Eq, Show)

instance FromJSON RemoveEmptySubscriptionResponses where
  parseJSON =
    withBool "RemoveEmptySubscriptionResponses"
      $ pure
      . \case
        True -> RemoveEmptyResponses
        False -> PreserveEmptyResponses

instance ToJSON RemoveEmptySubscriptionResponses where
  toJSON = \case
    RemoveEmptyResponses -> Bool True
    PreserveEmptyResponses -> Bool False
