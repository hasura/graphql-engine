{-# LANGUAGE StrictData #-}

module Hasura.GraphQL.Context
  ( RoleContext(..)
  , GQLContext(..)
  , ParserFn
  , RootField(..)
  , QueryDB(..)
  , MutationDB(..)
  , ActionQuery(..)
  , ActionMutation(..)
  , RemoteFieldG (..)
  , RemoteField
  , QueryRootField
  , MutationRootField
  , SubscriptionRootField
  , QueryDBRoot(..)
  , MutationDBRoot(..)
  , traverseActionQuery
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Language.GraphQL.Draft.Syntax as G

import           Data.Aeson.TH
import           Data.Typeable                 (Typeable)

import qualified Hasura.RQL.IR.Delete          as IR
import qualified Hasura.RQL.IR.Insert          as IR
import qualified Hasura.RQL.IR.Select          as IR
import qualified Hasura.RQL.IR.Update          as IR
import qualified Hasura.RQL.Types.Action       as RQL
import qualified Hasura.RQL.Types.Backend      as RQL
import qualified Hasura.RQL.Types.Common       as RQL
import qualified Hasura.RQL.Types.RemoteSchema as RQL

import           Hasura.GraphQL.Parser
import           Hasura.SQL.Backend


-- | For storing both a normal GQLContext and one for the backend variant.
-- Currently, this is to enable the backend variant to have certain insert
-- permissions which the frontend variant does not.

data RoleContext a
  = RoleContext
  { _rctxDefault :: !a -- ^ The default context for normal sessions
  , _rctxBackend :: !(Maybe a) -- ^ The context for sessions with backend privilege.
  } deriving (Show, Eq, Functor, Foldable, Traversable)
$(deriveToJSON hasuraJSON ''RoleContext)

data GQLContext = GQLContext
  { gqlQueryParser    :: ParserFn (InsOrdHashMap G.Name (QueryRootField UnpreparedValue))
  , gqlMutationParser :: Maybe (ParserFn (InsOrdHashMap G.Name (MutationRootField UnpreparedValue)))
  }

instance J.ToJSON GQLContext where
  toJSON GQLContext{} = J.String "The GraphQL schema parsers"

type ParserFn a
  =  G.SelectionSet G.NoFragments Variable
  -> Either (NESeq ParseError) (a, QueryReusability)

data RootField db remote action raw where
  RFDB
    :: forall (b :: BackendType) db remote action raw
     . (RQL.Backend b, Typeable db)
    => RQL.SourceName
    -> RQL.SourceConfig b
    -> db b
    -> RootField db remote action raw
  RFRemote :: remote -> RootField db remote action raw
  RFAction :: action -> RootField db remote action raw
  RFRaw    :: raw    -> RootField db remote action raw

data QueryDB (b :: BackendType) v
  = QDBMultipleRows (IR.AnnSimpleSelG       b v)
  | QDBSingleRow    (IR.AnnSimpleSelG       b v)
  | QDBAggregation  (IR.AnnAggregateSelectG b v)
  | QDBConnection   (IR.ConnectionSelect    b v)

data MutationDB (b :: BackendType) v
  = MDBInsert (IR.AnnInsert b v)
  | MDBUpdate (IR.AnnUpdG   b v)
  | MDBDelete (IR.AnnDelG   b v)
  | MDBFunction RQL.JsonAggSelect (IR.AnnSimpleSelG b v)
  -- ^ This represents a VOLATILE function, and is AnnSimpleSelG for easy
  -- re-use of non-VOLATILE function tracking code.

data ActionQuery (b :: BackendType) v
  = AQQuery !(RQL.AnnActionExecution  b v)
  | AQAsync !(RQL.AnnActionAsyncQuery b v)

data ActionMutation (b :: BackendType) v
  = AMSync !(RQL.AnnActionExecution b v)
  | AMAsync !RQL.AnnActionMutationAsync

data RemoteFieldG var
  = RemoteFieldG
  { _rfRemoteSchemaInfo :: !RQL.RemoteSchemaInfo
  , _rfField            :: !(G.Field G.NoFragments var)
  } deriving (Functor, Foldable, Traversable)

type RemoteField = RemoteFieldG RQL.RemoteSchemaVariable

-- The `db` type argument of @RootField@ expects only one type argument, the backend `b`, as not all
-- types stored in a RootField will have a second parameter like @QueryDB@ does: they all only have
-- in common the fact that they're parametric over the backend. To define @QueryRootField@ in terms
-- of @QueryDB@ (and likewise for mutations), we need a type-level function `b -> QueryDB b (v
-- b)`. Sadly, neither type synonyms nor type families may be partially applied. Hence the need for
-- @QueryDBRoot@ and @MutationDBRoot@.
newtype QueryDBRoot    v b = QDBR (QueryDB    b (v b))
newtype MutationDBRoot v b = MDBR (MutationDB b (v b))


type QueryRootField        v = RootField (QueryDBRoot    v) RemoteField (ActionQuery    'Postgres (v 'Postgres)) J.Value
type MutationRootField     v = RootField (MutationDBRoot v) RemoteField (ActionMutation 'Postgres (v 'Postgres)) J.Value
type SubscriptionRootField v = RootField (QueryDBRoot    v) Void Void Void


traverseActionQuery
  :: Applicative f
  => (a -> f b)
  -> ActionQuery backend a
  -> f (ActionQuery backend b)
traverseActionQuery f = \case
  AQQuery actionExecution -> AQQuery <$> RQL.traverseAnnActionExecution  f actionExecution
  AQAsync actionAsyncQ    -> AQAsync <$> RQL.traverseAnnActionAsyncQuery f actionAsyncQ
