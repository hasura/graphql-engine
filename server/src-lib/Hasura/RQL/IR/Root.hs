module Hasura.RQL.IR.Root
  ( RootField(..)
  , QueryDB(..)
  , MutationDB(..)
  , ActionQuery(..)
  , ActionMutation(..)
  , DBField(..)
  , QueryRootField
  , QueryDBRootField
  , QueryDBOnlyField
  , QueryActionRootField
  , QueryActionOnlyField
  , MutationRootField
  , MutationDBRootField
  , MutationDBOnlyField
  , MutationActionRootField
  , MutationActionOnlyField
  , SubscriptionRootField
  , QueryDBRoot(..)
  , MutationDBRoot(..)
  , traverseActionQuery
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

data DBField (field :: BackendType -> Type) (b :: BackendType)
  = DBField
  { _dfSourceName   :: !RQL.SourceName
  , _dfSourceConfig :: !(RQL.SourceConfig b)
  , _dfField        :: !(field b)
  }

data RootField db remote action raw where
  RFDB     :: db -> RootField db remote action raw
  RFRemote :: remote -> RootField db remote action raw
  RFAction :: action -> RootField db remote action raw
  RFRaw    :: raw    -> RootField db remote action raw


data MutationDB (b :: BackendType) (r :: BackendType -> Type) v
  = MDBInsert (AnnInsert b r v)
  | MDBUpdate (AnnUpdG   b r v)
  | MDBDelete (AnnDelG   b r v)
  | MDBFunction RQL.JsonAggSelect (AnnSimpleSelG b r v)
  -- ^ This represents a VOLATILE function, and is AnnSimpleSelG for easy
  -- re-use of non-VOLATILE function tracking code.
  deriving stock (Generic)

data ActionQuery (b :: BackendType) (r :: BackendType -> Type) v
  = AQQuery !(RQL.AnnActionExecution  b r v)
  | AQAsync !(RQL.AnnActionAsyncQuery b r v)

data ActionMutation (b :: BackendType) (r :: BackendType -> Type) v
  = AMSync !(RQL.AnnActionExecution b r v)
  | AMAsync !RQL.AnnActionMutationAsync

-- The `db` type argument of @RootField@ expects only one type argument, the
-- backend `b`, as not all types stored in a RootField will have a second
-- parameter like @QueryDB@ does: they all only have in common the fact that
-- they're parametric over the backend. To define @QueryRootField@ in terms of
-- @QueryDB@ (and likewise for mutations), we need a type-level function `b ->
-- QueryDB b (v b)`. Sadly, neither type synonyms nor type families may be
-- partially applied. Hence the need for @QueryDBRoot@ and @MutationDBRoot@.
newtype QueryDBRoot    r v b = QDBR (QueryDB    b r (v b))
newtype MutationDBRoot r v b = MDBR (MutationDB b r (v b))

type QueryDBRootField v = AB.AnyBackend (DBField (QueryDBRoot (RemoteSelect v) v))
type QueryDBOnlyField v = AB.AnyBackend (DBField (QueryDBRoot (Const Void) v))
type MutationDBRootField v = AB.AnyBackend (DBField (MutationDBRoot (RemoteSelect v) v))
type MutationDBOnlyField v = AB.AnyBackend (DBField (MutationDBRoot (Const Void) v))

type QueryActionRootField v =
  ActionQuery ('Postgres 'Vanilla) (RemoteSelect v) (v ('Postgres 'Vanilla))
type QueryActionOnlyField v =
  ActionQuery ('Postgres 'Vanilla) (Const Void) (v ('Postgres 'Vanilla))
type MutationActionRootField v =
  ActionMutation ('Postgres 'Vanilla) (RemoteSelect v) (v ('Postgres 'Vanilla))
type MutationActionOnlyField v =
  ActionMutation ('Postgres 'Vanilla) (Const Void) (v ('Postgres 'Vanilla))

type QueryRootField v =
  RootField (QueryDBRootField v) RQL.RemoteField (QueryActionRootField v) JO.Value
type MutationRootField v =
  RootField (MutationDBRootField v) RQL.RemoteField (MutationActionRootField v) JO.Value
type SubscriptionRootField v =
  RootField (QueryDBRootField v) Void Void Void

traverseActionQuery
  :: forall backend r f a b
   . (Applicative f, RQL.Backend backend)
  => (a -> f b)
  -> ActionQuery backend r a
  -> f (ActionQuery backend r b)
traverseActionQuery f = \case
  AQQuery actionExecution -> AQQuery <$> RQL.traverseAnnActionExecution  f actionExecution
  AQAsync actionAsyncQ    -> AQAsync <$> RQL.traverseAnnActionAsyncQuery f actionAsyncQ
