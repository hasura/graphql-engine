-- |
-- Definitions for schema building configuration.
module Hasura.GraphQL.Schema.Options
  ( SchemaOptions (..),
    StringifyNumbers (..),
    DangerouslyCollapseBooleans (..),
    InferFunctionPermissions (..),
    RemoteSchemaPermissions (..),
    OptimizePermissionFilters (..),
    IncludeAggregationPredicates (..),
    IncludeStreamFields (..),
    IncludeUpdateManyFields (..),
    BigQueryStringNumericInput (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withBool)
import Hasura.Prelude

-- | The record of options required to build the schema. This includes options
-- that can be enabled with 'Hasura.Server.Types.ExperimentalFeature' flags.
data SchemaOptions = SchemaOptions
  { soStringifyNumbers :: StringifyNumbers,
    soDangerousBooleanCollapse :: DangerouslyCollapseBooleans,
    soInferFunctionPermissions :: InferFunctionPermissions,
    soOptimizePermissionFilters :: OptimizePermissionFilters,
    soIncludeUpdateManyFields :: IncludeUpdateManyFields,
    soIncludeAggregationPredicates :: IncludeAggregationPredicates,
    soIncludeStreamFields :: IncludeStreamFields,
    soBigQueryStringNumericInput :: BigQueryStringNumericInput
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
    withBool "DangerouslyCollapseBooleans" $
      pure . \case
        True -> DangerouslyCollapseBooleans
        False -> Don'tDangerouslyCollapseBooleans

instance ToJSON DangerouslyCollapseBooleans where
  toJSON = \case
    DangerouslyCollapseBooleans -> Bool True
    Don'tDangerouslyCollapseBooleans -> Bool False

-- | Should we infer function permissions? If this flag is set to
-- 'InferFunctionPermissions', we may fail to build expression parsers
-- in 'buildQueryAndSubscriptionFields' for users with unrecognised roles.
data InferFunctionPermissions
  = InferFunctionPermissions
  | Don'tInferFunctionPermissions
  deriving (Eq, Show)

instance FromJSON InferFunctionPermissions where
  parseJSON =
    withBool "InferFunctionPermissions" $
      pure . \case
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
    withBool "RemoteSchemaPermissions" $
      pure . \case
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
