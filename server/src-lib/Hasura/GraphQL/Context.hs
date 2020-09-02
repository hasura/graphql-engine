{-# LANGUAGE StrictData #-}

module Hasura.GraphQL.Context
  ( RoleContext(..)
  , GQLContext(..)
  , ParserFn
  , RootField(..)
  , traverseDB
  , traverseMySQL
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

data RootField pgdb mydb remote action raw
  = RFPostgres pgdb
  | RFMySQL mydb
  | RFRemote remote
  | RFAction action
  | RFRaw raw

traverseDB :: forall pgdb pgdb' mydb remote action raw f
        . Applicative f
       => (pgdb -> f pgdb')
       -> RootField pgdb mydb remote action raw
       -> f (RootField pgdb' mydb remote action raw)
traverseDB f = \case
  RFPostgres x -> RFPostgres <$> f x
  RFMySQL x  -> pure $ RFMySQL x
  RFRemote x -> pure $ RFRemote x
  RFAction x -> pure $ RFAction x
  RFRaw x -> pure $ RFRaw x

traverseMySQL :: forall pgdb mydb mydb' remote action raw f
        . Applicative f
       => (mydb -> f mydb')
       -> RootField pgdb mydb remote action raw
       -> f (RootField pgdb mydb' remote action raw)
traverseMySQL f = \case
  RFMySQL x  -> RFMySQL <$> f x
  RFPostgres x -> pure $ RFPostgres x
  RFRemote x -> pure $ RFRemote x
  RFAction x -> pure $ RFAction x
  RFRaw x -> pure $ RFRaw x

traverseAction :: forall pgdb mydb remote action action' raw f
        . Applicative f
       => (action -> f action')
       -> RootField pgdb mydb remote action raw
       -> f (RootField pgdb mydb remote action' raw)
traverseAction f = \case
  RFPostgres x -> pure $ RFPostgres x
  RFMySQL    x -> pure $ RFMySQL x
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

type QueryRootField v = RootField (QueryDB v) (QueryDB v) RemoteField (ActionQuery v) J.Value

data MutationDB v
  = MDBInsert (AnnInsert   v)
  | MDBUpdate (RQL.AnnUpdG v)
  | MDBDelete (RQL.AnnDelG v)

data ActionMutation v
  = AMSync !(RQL.AnnActionExecution v)
  | AMAsync !RQL.AnnActionMutationAsync

type MutationRootField v =
  RootField (MutationDB v) (MutationDB v) RemoteField (ActionMutation v) J.Value

type SubscriptionRootField v = RootField (QueryDB v) Void Void (RQL.AnnActionAsyncQuery v) Void
type SubscriptionRootFieldResolved = RootField (QueryDB S.SQLExp) Void Void RQL.AnnSimpleSel Void
