{-# LANGUAGE StrictData #-}

module Hasura.GraphQL.Context
  ( RoleContext(..)
  , GQLContext(..)
  , ParserFn
  , RootField(..)
  , traverseDB
  , traverseAction
  , traverseRemoteField
  , QueryDB(..)
  , ActionQuery(..)
  , QueryRootField
  , MutationDB(..)
  , ActionMutation(..)
  , MutationRootField
  , SubscriptionRootField
  , SubscriptionRootFieldResolved
  , RemoteFieldG (..)
  , RemoteField
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                          as J
import qualified Language.GraphQL.Draft.Syntax       as G

import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Hasura.SQL.Backend

import qualified Hasura.Backends.Postgres.Connection as PG
import qualified Hasura.Backends.Postgres.SQL.DML    as PG
import qualified Hasura.RQL.IR.Delete                as IR
import qualified Hasura.RQL.IR.Insert                as IR
import qualified Hasura.RQL.IR.Select                as IR
import qualified Hasura.RQL.IR.Update                as IR
import qualified Hasura.RQL.Types.Action             as RQL
import qualified Hasura.RQL.Types.Common             as RQL
import qualified Hasura.RQL.Types.RemoteSchema       as RQL

import           Hasura.GraphQL.Parser

-- | For storing both a normal GQLContext and one for the backend variant.
-- Currently, this is to enable the backend variant to have certain insert
-- permissions which the frontend variant does not.

data RoleContext a
  = RoleContext
  { _rctxDefault :: !a -- ^ The default context for normal sessions
  , _rctxBackend :: !(Maybe a) -- ^ The context for sessions with backend privilege.
  } deriving (Show, Eq, Functor, Foldable, Traversable)
$(deriveToJSON (aesonDrop 5 snakeCase) ''RoleContext)

data GQLContext = GQLContext
  { gqlQueryParser    :: ParserFn (InsOrdHashMap G.Name (QueryRootField (UnpreparedValue 'Postgres)))
  , gqlMutationParser :: Maybe (ParserFn (InsOrdHashMap G.Name (MutationRootField (UnpreparedValue 'Postgres))))
  }

instance J.ToJSON GQLContext where
  toJSON GQLContext{} = J.String "The GraphQL schema parsers"

type ParserFn a
  =  G.SelectionSet G.NoFragments Variable
  -> Either (NESeq ParseError) (a, QueryReusability)

data RootField db remote action raw
  = RFDB !RQL.SourceName !PG.PGExecCtx db
  | RFRemote remote
  | RFAction action
  | RFRaw raw

traverseDB :: forall db db' remote action raw f
        . Applicative f
       => (db -> f db')
       -> RootField db remote action raw
       -> f (RootField db' remote action raw)
traverseDB f = \case
  RFDB s e x -> RFDB s e <$> f x
  RFRemote x -> pure $ RFRemote x
  RFAction x -> pure $ RFAction x
  RFRaw x    -> pure $ RFRaw x

traverseAction :: forall db remote action action' raw f
        . Applicative f
       => (action -> f action')
       -> RootField db remote action raw
       -> f (RootField db remote action' raw)
traverseAction f = \case
  RFDB s e x -> pure $ RFDB s e x
  RFRemote x -> pure $ RFRemote x
  RFAction x -> RFAction <$> f x
  RFRaw x    -> pure $ RFRaw x

traverseRemoteField :: forall db remote remote' action raw f
        . Applicative f
       => (remote -> f remote')
       -> RootField db remote action raw
       -> f (RootField db remote' action raw)
traverseRemoteField f = \case
  RFDB s e x -> pure $ RFDB s e x
  RFRemote x -> RFRemote <$> f x
  RFAction x -> pure $ RFAction x
  RFRaw x -> pure $ RFRaw x

data QueryDB b v
  = QDBSimple      (IR.AnnSimpleSelG       b v)
  | QDBPrimaryKey  (IR.AnnSimpleSelG       b v)
  | QDBAggregation (IR.AnnAggregateSelectG b v)
  | QDBConnection  (IR.ConnectionSelect    b v)

data ActionQuery (b :: BackendType) v
  = AQQuery !(RQL.AnnActionExecution b v)
  | AQAsync !(RQL.AnnActionAsyncQuery b v)

data RemoteFieldG var
  = RemoteFieldG
  { _rfRemoteSchemaInfo :: !RQL.RemoteSchemaInfo
  , _rfField            :: !(G.Field G.NoFragments var)
  } deriving (Functor, Foldable, Traversable)

type RemoteField = RemoteFieldG RQL.RemoteSchemaVariable

type QueryRootField v = RootField (QueryDB 'Postgres v) RemoteField (ActionQuery 'Postgres v) J.Value

data MutationDB (b :: BackendType) v
  = MDBInsert (IR.AnnInsert   b v)
  | MDBUpdate (IR.AnnUpdG b v)
  | MDBDelete (IR.AnnDelG b v)
  | MDBFunction (IR.AnnSimpleSelG b v)
  -- ^ This represents a VOLATILE function, and is AnnSimpleSelG for easy
  -- re-use of non-VOLATILE function tracking code.

data ActionMutation (b :: BackendType) v
  = AMSync !(RQL.AnnActionExecution b v)
  | AMAsync !RQL.AnnActionMutationAsync

type MutationRootField v =
  RootField (MutationDB 'Postgres v) RemoteField (ActionMutation 'Postgres v) J.Value

type SubscriptionRootField v = RootField (QueryDB 'Postgres v) Void Void Void
type SubscriptionRootFieldResolved = RootField (QueryDB 'Postgres PG.SQLExp) Void Void Void
