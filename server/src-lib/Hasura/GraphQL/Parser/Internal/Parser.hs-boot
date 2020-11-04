module Hasura.GraphQL.Parser.Internal.Parser where

import           Hasura.Prelude

import           Hasura.GraphQL.Parser.Class.Parse
import           Hasura.GraphQL.Parser.Internal.Types
import           Hasura.GraphQL.Parser.Schema

boolean :: MonadParse m => Parser 'Both m Bool
