{-# LANGUAGE StrictData #-}

module Hasura.GraphQL.Context
  ( RoleContext(..)
  , GQLContext(..)
  , ParserFn
  , RootField(..)
  , traverseDB
  , traverseAction
  , RemoteField
  , QueryDB(..)
  , ActionQuery(..)
  , QueryRootField
  , MutationDB(..)
  , ActionMutation(..)
  , MutationRootField
  , SubscriptionRootField
  , SubscriptionRootFieldResolved
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                    as J
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Hasura.SQL.Backend
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
  { gqlQueryParser    :: ParserFn (InsOrdHashMap G.Name (QueryRootField UnpreparedValue))
  , gqlMutationParser :: Maybe (ParserFn (InsOrdHashMap G.Name (MutationRootField UnpreparedValue)))
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

data QueryDB b v
  = QDBSimple      (RQL.AnnSimpleSelG       b v)
  | QDBPrimaryKey  (RQL.AnnSimpleSelG       b v)
  | QDBAggregation (RQL.AnnAggregateSelectG b v)
  | QDBConnection  (RQL.ConnectionSelect    b v)

data ActionQuery (b :: Backend) v
  = AQQuery !(RQL.AnnActionExecution b v)
  | AQAsync !(RQL.AnnActionAsyncQuery b v)

type RemoteField = (RQL.RemoteSchemaInfo, G.Field G.NoFragments G.Name)

type QueryRootField v = RootField (QueryDB 'Postgres v) RemoteField (ActionQuery 'Postgres v) J.Value

data MutationDB (b :: Backend) v
  = MDBInsert (AnnInsert   b v)
  | MDBUpdate (RQL.AnnUpdG b v)
  | MDBDelete (RQL.AnnDelG b v)

data ActionMutation (b :: Backend) v
  = AMSync !(RQL.AnnActionExecution b v)
  | AMAsync !RQL.AnnActionMutationAsync

type MutationRootField v =
  RootField (MutationDB 'Postgres v) RemoteField (ActionMutation 'Postgres v) J.Value

type SubscriptionRootField v = RootField (QueryDB 'Postgres v) Void (RQL.AnnActionAsyncQuery 'Postgres v) Void
type SubscriptionRootFieldResolved = RootField (QueryDB 'Postgres S.SQLExp) Void (RQL.AnnSimpleSel 'Postgres) Void
