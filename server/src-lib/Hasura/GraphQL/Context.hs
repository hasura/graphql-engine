{-# LANGUAGE StrictData #-}

module Hasura.GraphQL.Context
  ( GQLContext(..)
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
import qualified Language.GraphQL.Draft.Syntax as G

import           Data.HashMap.Strict.InsOrd    (InsOrdHashMap)

import qualified Hasura.RQL.DML.Delete.Types   as RQL
import qualified Hasura.RQL.DML.Select.Types   as RQL
import qualified Hasura.RQL.DML.Update.Types   as RQL
import qualified Hasura.RQL.Types.Action       as RQL
import qualified Hasura.RQL.Types.RemoteSchema as RQL
import qualified Hasura.SQL.DML                as S

import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Schema.Insert  (AnnMultiInsert)

data GQLContext = GQLContext
  { gqlQueryParser    :: ParserFn (InsOrdHashMap G.Name (QueryRootField UnpreparedValue))
  -- TODO should we make mutation (and later subscription) parsers Maybe?
  , gqlMutationParser :: ParserFn (InsOrdHashMap G.Name (MutationRootField UnpreparedValue))
  }

instance J.ToJSON GQLContext where
  toJSON GQLContext{} = J.object [] -- FIXME

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

data QueryDB v
  = QDBSimple      (RQL.AnnSimpleSelG v)
  | QDBPrimaryKey  (RQL.AnnSimpleSelG v)
  | QDBAggregation (RQL.AnnAggSelG    v)

data ActionQuery v
  = AQQuery !(RQL.AnnActionExecution v)
  | AQAsync !(RQL.AnnActionAsyncQuery v)

-- TODO this should maybe take a G.Field rather than a
-- G.TypedOperationDefinition -- the operation would get built when we pass to
-- the execution phase.
type RemoteField = (RQL.RemoteSchemaInfo, G.Field G.NoFragments Variable)

type QueryRootField v = RootField (QueryDB v) RemoteField (ActionQuery v) J.Value

data MutationDB v
  = MDBInsert (AnnMultiInsert v)
  | MDBUpdate (RQL.AnnUpdG    v)
  | MDBDelete (RQL.AnnDelG    v)

data ActionMutation v
  = AMSync !(RQL.AnnActionExecution v)
  | AMAsync !RQL.AnnActionMutationAsync

type MutationRootField v =
  RootField (MutationDB v) RemoteField (ActionMutation v) J.Value

type SubscriptionRootField v = RootField (QueryDB v) Void (RQL.AnnActionAsyncQuery v) Void
type SubscriptionRootFieldResolved = RootField (QueryDB S.SQLExp) Void (RQL.AnnSimpleSel) Void
