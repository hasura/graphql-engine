module Hasura.GraphQL.Parser.Collect where

import           Hasura.Prelude

import           Language.GraphQL.Draft.Syntax

import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Schema

collectFields
  :: (MonadParse m, Foldable t)
  => t Name
  -> SelectionSet NoFragments Variable
  -> m (InsOrdHashMap Name (Field NoFragments Variable))
