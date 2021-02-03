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
  , QueryRootField(..)
  , MutationDB(..)
  , ActionMutation(..)
  , MutationRootField(..)
  , SubscriptionRootField(..)
  , SubscriptionRootFieldResolved
  , RemoteFieldG (..)
  , RemoteField
  , rawQueryRootField
  , rawMutationRootField
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                       as J
import qualified Language.GraphQL.Draft.Syntax    as G

import           Data.Aeson.TH
import           Data.Typeable                    (Typeable)

import qualified Hasura.Backends.Postgres.SQL.DML as PG
import qualified Hasura.RQL.IR.Delete             as IR
import qualified Hasura.RQL.IR.Insert             as IR
import qualified Hasura.RQL.IR.Select             as IR
import qualified Hasura.RQL.IR.Update             as IR
import qualified Hasura.RQL.Types.Action          as RQL
import qualified Hasura.RQL.Types.Common          as RQL
import qualified Hasura.RQL.Types.RemoteSchema    as RQL

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

data RootField b db remote action raw
  = RFDB !RQL.SourceName (RQL.SourceConfig b) db
  | RFRemote remote
  | RFAction action
  | RFRaw raw

traverseDB :: forall b db db' remote action raw f
        . Applicative f
       => (db -> f db')
       -> RootField b db remote action raw
       -> f (RootField b db' remote action raw)
traverseDB f = \case
  RFDB s e x -> RFDB s e <$> f x
  RFRemote x -> pure $ RFRemote x
  RFAction x -> pure $ RFAction x
  RFRaw x    -> pure $ RFRaw x

traverseAction :: forall b db remote action action' raw f
        . Applicative f
       => (action -> f action')
       -> RootField b db remote action raw
       -> f (RootField b db remote action' raw)
traverseAction f = \case
  RFDB s e x -> pure $ RFDB s e x
  RFRemote x -> pure $ RFRemote x
  RFAction x -> RFAction <$> f x
  RFRaw x    -> pure $ RFRaw x

traverseRemoteField
  :: forall b db remote remote' action raw f
   . Applicative f
  => (remote -> f remote')
  -> RootField b db remote action raw
  -> f (RootField b db remote' action raw)
traverseRemoteField f = \case
  RFDB s e x -> pure $ RFDB s e x
  RFRemote x -> RFRemote <$> f x
  RFAction x -> pure $ RFAction x
  RFRaw x    -> pure $ RFRaw x

data QueryDB b v
  = QDBMultipleRows (IR.AnnSimpleSelG       b v)
  | QDBSingleRow    (IR.AnnSimpleSelG       b v)
  | QDBAggregation  (IR.AnnAggregateSelectG b v)
  | QDBConnection   (IR.ConnectionSelect    b v)

data ActionQuery (b :: BackendType) v
  = AQQuery !(RQL.AnnActionExecution b v)
  | AQAsync !(RQL.AnnActionAsyncQuery b v)

data RemoteFieldG var
  = RemoteFieldG
  { _rfRemoteSchemaInfo :: !RQL.RemoteSchemaInfo
  , _rfField            :: !(G.Field G.NoFragments var)
  } deriving (Functor, Foldable, Traversable)

type RemoteField = RemoteFieldG RQL.RemoteSchemaVariable

data QueryRootField v where
  QueryRootField
    :: forall b v
     . (RQL.Backend b, Typeable v)
    => RootField b (QueryDB b (v b)) RemoteField (ActionQuery 'Postgres {- FIXME -} (v 'Postgres)) J.Value
    -> QueryRootField v

data MutationRootField v where
  MutationRootField
    :: forall b v
     . (RQL.Backend b, Typeable v)
    => RootField b (MutationDB b (v b)) RemoteField (ActionMutation 'Postgres {- FIXME -} (v 'Postgres)) J.Value
    -> MutationRootField v

data SubscriptionRootField v where
  SubscriptionRootField
    :: forall b v
     . (RQL.Backend b, Typeable v)
    => RootField b (QueryDB b (v b)) Void Void Void
    -> SubscriptionRootField v


data MutationDB (b :: BackendType) v
  = MDBInsert (IR.AnnInsert   b v)
  | MDBUpdate (IR.AnnUpdG b v)
  | MDBDelete (IR.AnnDelG b v)
  | MDBFunction RQL.JsonAggSelect (IR.AnnSimpleSelG b v)
  -- ^ This represents a VOLATILE function, and is AnnSimpleSelG for easy
  -- re-use of non-VOLATILE function tracking code.

data ActionMutation (b :: BackendType) v
  = AMSync !(RQL.AnnActionExecution b v)
  | AMAsync !RQL.AnnActionMutationAsync


-- TODO: remove this
type SubscriptionRootFieldResolved = RootField 'Postgres (QueryDB 'Postgres PG.SQLExp) Void Void Void


-- RFRaw is not tied to any backend, but a backend must be provided as
-- a parameter to QueryRootField's constructor. We provide 'Postgres
-- for historical reasons.
rawQueryRootField :: Typeable v => J.Value -> QueryRootField v
rawQueryRootField = QueryRootField @'Postgres . RFRaw

rawMutationRootField :: Typeable v => J.Value -> MutationRootField v
rawMutationRootField = MutationRootField @'Postgres . RFRaw
