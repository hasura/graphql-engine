{-# LANGUAGE StrictData #-}

module Hasura.GraphQL.Context
  ( RoleContext(..)
  , GQLContext(..)
  , ParserFn
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Language.GraphQL.Draft.Syntax as G

import           Data.Aeson.TH

import qualified Hasura.RQL.IR                 as IR

import           Hasura.GraphQL.Parser


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
  { gqlQueryParser    ::        ParserFn (InsOrdHashMap G.Name (IR.QueryRootField    UnpreparedValue UnpreparedValue))
  , gqlMutationParser :: Maybe (ParserFn (InsOrdHashMap G.Name (IR.MutationRootField UnpreparedValue UnpreparedValue)))
  }

instance J.ToJSON GQLContext where
  toJSON GQLContext{} = J.String "The GraphQL schema parsers"

type ParserFn a
  =  G.SelectionSet G.NoFragments Variable
  -> Either (NESeq ParseError) (a, QueryReusability)
