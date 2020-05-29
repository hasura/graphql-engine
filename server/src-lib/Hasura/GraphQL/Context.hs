{-# LANGUAGE StrictData #-}

module Hasura.GraphQL.Context
  ( GQLContext(..)
  , ParserFn
  , QueryRootField(..)
  , MutationRootField(..)
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Language.GraphQL.Draft.Syntax as G

import           Data.HashMap.Strict.InsOrd    (InsOrdHashMap)

import qualified Hasura.RQL.DML.Delete.Types   as RQL
import qualified Hasura.RQL.DML.Select.Types   as RQL
import qualified Hasura.RQL.DML.Update.Types   as RQL

import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Schema.Insert  (AnnMultiInsert)

data GQLContext = GQLContext
  { gqlQueryParser :: ParserFn (InsOrdHashMap G.Name (QueryRootField UnpreparedValue))
  }

instance J.ToJSON GQLContext where
  toJSON GQLContext{} = J.object [] -- FIXME

type ParserFn a
  =  G.SelectionSet G.NoFragments Variable
  -> Either (NESeq ParseError) (a, QueryReusability)

-- FIXME: taken from Resolve.hs
-- do we want to keep it the same?
data QueryRootField v
  = QRFSimple      (RQL.AnnSimpleSelG v)
  | QRFPrimaryKey  (RQL.AnnSimpleSelG v)
  | QRFAggregation (RQL.AnnAggSelG    v)
  | QRFRaw         J.Value

data MutationRootField v
  = MRFInsert      (AnnMultiInsert v)
  | MRFUpdate      (RQL.AnnUpdG    v)
  | MRFDelete      (RQL.AnnDelG    v)
  | MRFRaw         J.Value
