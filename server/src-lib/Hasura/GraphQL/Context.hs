{-# LANGUAGE StrictData #-}

module Hasura.GraphQL.Context
  ( GQLContext(..)
  , ParserFn
  , RootField(..)
  , traverseDB
  , QueryDB(..)
  , QueryRootField
  , MutationDB(..)
  , MutationRootField
  , SubscriptionRootField
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Language.GraphQL.Draft.Syntax as G

import           Data.HashMap.Strict.InsOrd    (InsOrdHashMap)

import qualified Hasura.RQL.DML.Delete.Types   as RQL
import qualified Hasura.RQL.DML.Select.Types   as RQL
import qualified Hasura.RQL.DML.Update.Types   as RQL
import qualified Hasura.RQL.Types.RemoteSchema as RQL

import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Schema.Insert  (AnnMultiInsert)

data GQLContext = GQLContext
  { gqlQueryParser :: ParserFn (InsOrdHashMap G.Name (QueryRootField UnpreparedValue))
  -- TODO should we make mutation (and later subscription) parsers Maybe?
  , gqlMutationParser :: ParserFn (InsOrdHashMap G.Name (MutationRootField UnpreparedValue))
  }

instance J.ToJSON GQLContext where
  toJSON GQLContext{} = J.object [] -- FIXME

type ParserFn a
  =  G.SelectionSet G.NoFragments Variable
  -> Either (NESeq ParseError) (a, QueryReusability)

data RootField db remote raw
  = RFDB db
  | RFRemote remote
  | RFRaw raw

traverseDB :: forall db db' remote raw f
        . Applicative f
       => (db -> f db')
       -> RootField db  remote raw
       -> f (RootField db' remote raw)
traverseDB f = \case
  RFDB x -> RFDB <$> f x
  RFRemote x -> pure $ RFRemote x
  RFRaw x -> pure $ RFRaw x

data QueryDB v
  = QDBSimple      (RQL.AnnSimpleSelG v)
  | QDBPrimaryKey  (RQL.AnnSimpleSelG v)
  | QDBAggregation (RQL.AnnAggSelG    v)

-- TODO this should maybe take a G.Field rather than a
-- G.TypedOperationDefinition -- the operation would get built when we pass to
-- the execution phase.
type RemoteField = (RQL.RemoteSchemaInfo, G.TypedOperationDefinition G.FragmentSpread G.Name)

type QueryRootField v = RootField (QueryDB v) RemoteField J.Value

data MutationDB v
  = MDBInsert (AnnMultiInsert v)
  | MDBUpdate (RQL.AnnUpdG    v)
  | MDBDelete (RQL.AnnDelG    v)

type MutationRootField v = RootField (MutationDB v) RemoteField J.Value

type SubscriptionRootField v = RootField (QueryDB v) Void Void
