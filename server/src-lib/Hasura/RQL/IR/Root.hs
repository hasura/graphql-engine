module Hasura.RQL.IR.Root
  ( SourceConfigWith(..)
  , RootField(..)
  , MutationDB(..)
  , ActionQuery(..)
  , ActionMutation(..)
  , QueryRootField
  , MutationRootField
  , SubscriptionRootField
  , QueryDBRoot(..)
  , MutationDBRoot(..)
  ) where

import           Hasura.Prelude

import qualified Data.Aeson.Ordered            as JO

import           Data.Kind                     (Type)

import qualified Hasura.RQL.Types.Action       as RQL
import qualified Hasura.RQL.Types.Backend      as RQL
import qualified Hasura.RQL.Types.Common       as RQL
import qualified Hasura.RQL.Types.RemoteSchema as RQL
import qualified Hasura.SQL.AnyBackend         as AB

import           Hasura.RQL.IR.Delete
import           Hasura.RQL.IR.Insert
import           Hasura.RQL.IR.Select
import           Hasura.RQL.IR.Update
import           Hasura.SQL.Backend


data SourceConfigWith (db :: BackendType -> Type) (b :: BackendType) =
  SourceConfigWith (RQL.SourceConfig b) (db b)

data RootField (db :: BackendType -> Type) remote action raw where
  RFDB
    :: RQL.SourceName
    -> AB.AnyBackend (SourceConfigWith db)
    -> RootField db remote action raw
  RFRemote :: remote -> RootField db remote action raw
  RFAction :: action -> RootField db remote action raw
  RFRaw    :: raw    -> RootField db remote action raw


data MutationDB (b :: BackendType) (r :: BackendType -> Type) v
  = MDBInsert (AnnInsert b r v)
  | MDBUpdate (AnnUpdG   b r v)
  | MDBDelete (AnnDelG   b r v)
  | MDBFunction RQL.JsonAggSelect (AnnSimpleSelectG b r v)
  -- ^ This represents a VOLATILE function, and is AnnSimpleSelG for easy
  -- re-use of non-VOLATILE function tracking code.
  deriving stock (Generic, Functor, Foldable, Traversable)

data ActionQuery (b :: BackendType) (r :: BackendType -> Type) v
  = AQQuery !(RQL.AnnActionExecution  b r v)
  | AQAsync !(RQL.AnnActionAsyncQuery b r v)
  deriving (Functor, Foldable, Traversable)

data ActionMutation (b :: BackendType) (r :: BackendType -> Type) v
  = AMSync !(RQL.AnnActionExecution b r v)
  | AMAsync !RQL.AnnActionMutationAsync
  deriving (Functor, Foldable, Traversable)


-- The `db` type argument of @RootField@ expects only one type argument, the backend `b`, as not all
-- types stored in a RootField will have a second parameter like @QueryDB@ does: they all only have
-- in common the fact that they're parametric over the backend. To define @QueryRootField@ in terms
-- of @QueryDB@ (and likewise for mutations), we need a type-level function `b -> QueryDB b (v
-- b)`. Sadly, neither type synonyms nor type families may be partially applied. Hence the need for
-- @QueryDBRoot@ and @MutationDBRoot@.
newtype QueryDBRoot    r v b = QDBR (QueryDB    b r (v b))
newtype MutationDBRoot r v b = MDBR (MutationDB b r (v b))


type QueryRootField        r v = RootField (QueryDBRoot    r v) RQL.RemoteField (ActionQuery    ('Postgres 'Vanilla) r (v ('Postgres 'Vanilla))) JO.Value
type MutationRootField     r v = RootField (MutationDBRoot r v) RQL.RemoteField (ActionMutation ('Postgres 'Vanilla) r (v ('Postgres 'Vanilla))) JO.Value
type SubscriptionRootField r v = RootField (QueryDBRoot    r v) Void            Void                                                             Void
