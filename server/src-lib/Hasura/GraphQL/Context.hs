module Hasura.GraphQL.Context
  ( RoleContext (..),
    GQLContext (..),
    ParserFn,
  )
where

import Data.Aeson qualified as J
import Hasura.Base.Error
import Hasura.GraphQL.Namespace
import Hasura.GraphQL.Parser
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Language.GraphQL.Draft.Syntax qualified as G

-- | For storing both a normal GQLContext and one for the backend variant.
-- Currently, this is to enable the backend variant to have certain insert/update/delete
-- permissions which the frontend variant does not.
data RoleContext a = RoleContext
  { -- | The default context for normal sessions
    _rctxDefault :: !a,
    -- | The context for sessions with backend privilege.
    _rctxBackend :: !(Maybe a)
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance (J.ToJSON a) => J.ToJSON (RoleContext a) where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

data GQLContext = GQLContext
  { gqlQueryParser :: ParserFn (RootFieldMap (IR.QueryRootField IR.UnpreparedValue)),
    gqlMutationParser :: Maybe (ParserFn (RootFieldMap (IR.MutationRootField IR.UnpreparedValue))),
    gqlSubscriptionParser :: Maybe (ParserFn (RootFieldMap (IR.QueryRootField IR.UnpreparedValue)))
  }

instance J.ToJSON GQLContext where
  toJSON GQLContext {} = J.String "The GraphQL schema parsers"

type ParserFn a =
  G.SelectionSet G.NoFragments Variable ->
  Either QErr a
