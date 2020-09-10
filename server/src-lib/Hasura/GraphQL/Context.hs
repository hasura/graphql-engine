{-# LANGUAGE StrictData #-}

module Hasura.GraphQL.Context
  ( RoleContext(..)
  , GQLContext(..)
  , ParserFn
  , RootField(..)
  , traverseDB
  , traverseAction
  , RemoteField
  , RootTree(..)
  , RootJoin(..)
  , QueryDB(..)
  , ActionQuery(..)
  , QueryRootTree
  , QueryRootJoin
  , MutationDB(..)
  , ActionMutation(..)
  , MutationRootTree
  , MutationRootJoin
  , SubscriptionRootField
  , SubscriptionRootFieldResolved
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                    as J
import           Data.Aeson.Casing
import           Data.Aeson.TH
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.RQL.DML.Delete.Types   as RQL
import qualified Hasura.RQL.DML.Select.Types   as RQL
import qualified Hasura.RQL.DML.Update.Types   as RQL
import qualified Hasura.RQL.Types.Action       as RQL
import qualified Hasura.RQL.Types.RemoteSchema as RQL
import qualified Hasura.SQL.DML                as S

import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Schema.Insert  (AnnInsert)

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
  { gqlQueryParser    :: ParserFn (InsOrdHashMap G.Name (QueryRootTree UnpreparedValue))
  , gqlMutationParser :: Maybe (ParserFn (InsOrdHashMap G.Name (MutationRootTree UnpreparedValue)))
  }

instance J.ToJSON GQLContext where
  toJSON GQLContext{} = J.String "The GraphQL schema parsers"

type ParserFn a
  =  G.SelectionSet G.NoFragments Variable
  -> Either (NESeq ParseError) (a, QueryReusability)

data RootField db remote action raw
  = RFDB db
  | RFRemote remote
  | RFAction action
  | RFRaw raw

data RootTree v = RootTree v [RootJoin v]
data RootJoin v = RootJoin (RootTree v) G.Name G.Name

traverseDB :: forall db db' remote action raw f
        . Applicative f
       => (db -> f db')
       -> RootField db remote action raw
       -> f (RootField db' remote action raw)
traverseDB f = \case
  RFDB x -> RFDB <$> f x
  RFRemote x -> pure $ RFRemote x
  RFAction x -> pure $ RFAction x
  RFRaw x -> pure $ RFRaw x

traverseAction :: forall db remote action action' raw f
        . Applicative f
       => (action -> f action')
       -> RootField db remote action raw
       -> f (RootField db remote action' raw)
traverseAction f = \case
  RFDB x -> pure $ RFDB x
  RFRemote x -> pure $ RFRemote x
  RFAction x -> RFAction <$> f x
  RFRaw x -> pure $ RFRaw x

data QueryDB v
  = QDBSimple      (RQL.AnnSimpleSelG       v)
  | QDBPrimaryKey  (RQL.AnnSimpleSelG       v)
  | QDBAggregation (RQL.AnnAggregateSelectG v)
  | QDBConnection  (RQL.ConnectionSelect    v)

data ActionQuery v
  = AQQuery !(RQL.AnnActionExecution v)
  | AQAsync !(RQL.AnnActionAsyncQuery v)

type RemoteField = (RQL.RemoteSchemaInfo, G.Field G.NoFragments Variable)

type QueryRootTree v = RootTree (RootField (QueryDB v) RemoteField (ActionQuery v) J.Value)
type QueryRootJoin v = RootJoin (RootField (QueryDB v) RemoteField (ActionQuery v) J.Value)

data MutationDB v
  = MDBInsert (AnnInsert   v)
  | MDBUpdate (RQL.AnnUpdG v)
  | MDBDelete (RQL.AnnDelG v)

data ActionMutation v
  = AMSync !(RQL.AnnActionExecution v)
  | AMAsync !RQL.AnnActionMutationAsync

type MutationRootTree v =
  RootTree (RootField (MutationDB v) RemoteField (ActionMutation v) J.Value)
type MutationRootJoin v =
  RootJoin (RootField (MutationDB v) RemoteField (ActionMutation v) J.Value)

type SubscriptionRootField v = RootField (QueryDB v) Void (RQL.AnnActionAsyncQuery v) Void
type SubscriptionRootFieldResolved = RootField (QueryDB S.SQLExp) Void RQL.AnnSimpleSel Void
