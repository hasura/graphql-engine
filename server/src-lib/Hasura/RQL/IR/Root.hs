{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.IR.Root
  ( SourceConfigWith (..),
    RootField (..),
    MutationDB (..),
    ActionQuery (..),
    ActionMutation (..),
    QueryRootField,
    MutationRootField,
    SubscriptionRootField,
    QueryDBRoot (..),
    MutationDBRoot (..),
    RemoteRelationshipField (..),
  )
where

import Data.Aeson.Ordered qualified as JO
import Data.Kind (Type)
import Hasura.Prelude
import Hasura.QueryTags.Types qualified as RQL
import Hasura.RQL.IR.Action
import Hasura.RQL.IR.Delete
import Hasura.RQL.IR.Insert
import Hasura.RQL.IR.RemoteSchema
import Hasura.RQL.IR.Select
import Hasura.RQL.IR.Update
import Hasura.RQL.Types.Backend qualified as RQL
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common qualified as RQL
import Hasura.RemoteSchema.Metadata.Base (RemoteSchemaName)
import Hasura.RemoteSchema.SchemaCache.Types qualified as RQL
import Hasura.SQL.AnyBackend qualified as AB

data SourceConfigWith (db :: BackendType -> Type) (b :: BackendType)
  = SourceConfigWith (RQL.SourceConfig b) (Maybe RQL.QueryTagsConfig) (db b)

data RootField (db :: BackendType -> Type) remote action raw where
  RFDB ::
    RQL.SourceName ->
    AB.AnyBackend (SourceConfigWith db) ->
    RootField db remote action raw
  RFRemote :: RemoteSchemaName -> remote -> RootField db remote action raw
  RFAction :: action -> RootField db remote action raw
  RFRaw :: raw -> RootField db remote action raw
  RFMulti :: [RootField db remote action raw] -> RootField db remote action raw

data MutationDB (b :: BackendType) (r :: Type) v
  = MDBInsert (AnnotatedInsert b r v)
  | MDBUpdate (AnnotatedUpdateG b r v)
  | MDBDelete (AnnDelG b r v)
  | -- | This represents a VOLATILE function, and is AnnSimpleSelG for easy
    -- re-use of non-VOLATILE function tracking code.
    MDBFunction RQL.JsonAggSelect (AnnSimpleSelectG b r v)
  deriving stock (Generic, Functor, Foldable, Traversable)

data ActionQuery (r :: Type)
  = AQQuery (AnnActionExecution r)
  | AQAsync (AnnActionAsyncQuery ('Postgres 'Vanilla) r)
  deriving stock (Functor, Foldable, Traversable)

data ActionMutation (r :: Type)
  = AMSync (AnnActionExecution r)
  | AMAsync AnnActionMutationAsync

-- The `db` type argument of @RootField@ expects only one type argument, the backend `b`, as not all
-- types stored in a RootField will have a second parameter like @QueryDB@ does: they all only have
-- in common the fact that they're parametric over the backend. To define @QueryRootField@ in terms
-- of @QueryDB@ (and likewise for mutations), we need a type-level function `b -> QueryDB b (v
-- b)`. Sadly, neither type synonyms nor type families may be partially applied. Hence the need for
-- @QueryDBRoot@ and @MutationDBRoot@.
newtype QueryDBRoot r v b = QDBR (QueryDB b r (v b))

newtype MutationDBRoot r v b = MDBR (MutationDB b r (v b))

-- | IR of a remote relationship. A remote relationship currently can be to
-- either a remote schema or a database's table. See RemoteSourceSelect for
-- explanation on 'vf'.
data RemoteRelationshipField vf
  = RemoteSchemaField (RemoteSchemaSelect (RemoteRelationshipField vf))
  | -- | AnyBackend is used here to capture a relationship to an arbitrary target
    RemoteSourceField (AB.AnyBackend (RemoteSourceSelect (RemoteRelationshipField vf) vf))

deriving instance (AB.SatisfiesForAllBackends vf Show) => Show (RemoteRelationshipField vf)

-- | Represents a query root field to an action
type QueryActionRoot v =
  ActionQuery (RemoteRelationshipField v)

-- | Represents a mutation root field to an action
type MutationActionRoot v =
  ActionMutation (RemoteRelationshipField v)

type QueryRootField v =
  RootField
    (QueryDBRoot (RemoteRelationshipField v) v)
    (RemoteSchemaRootField (RemoteRelationshipField v) RQL.RemoteSchemaVariable)
    (QueryActionRoot v)
    JO.Value

type MutationRootField v =
  RootField
    (MutationDBRoot (RemoteRelationshipField v) v)
    (RemoteSchemaRootField (RemoteRelationshipField v) RQL.RemoteSchemaVariable)
    (MutationActionRoot v)
    JO.Value

type SubscriptionRootField v =
  RootField
    (QueryDBRoot (RemoteRelationshipField v) v)
    (RemoteSchemaRootField (RemoteRelationshipField v) RQL.RemoteSchemaVariable)
    (QueryActionRoot v)
    JO.Value
