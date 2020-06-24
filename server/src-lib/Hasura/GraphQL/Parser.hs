-- | This module exports the public API to our internal GraphQL query parser
-- combinator language. For more details, see the documentation for 'Parser'.
module Hasura.GraphQL.Parser
  ( Parser
  , parserType
  , runParser
  , bind
  , bindFields

  , ScalarRepresentation(..)
  , scalar
  , boolean
  , int
  , float
  , string

  , enum
  , nullable
  , list
  , object
  , selectionSet

  , InputFieldsParser
  , field
  , fieldWithDefault
  , fieldOptional

  , FieldParser
  , ParsedSelection(..)
  , handleTypename
  , selection
  , selection_
  , subselection
  , subselection_

  , module Hasura.GraphQL.Parser.Class
  , module Hasura.GraphQL.Parser.Column
  , module Hasura.GraphQL.Parser.Monad
  , module Hasura.GraphQL.Parser.Schema
  ) where

import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Column
import           Hasura.GraphQL.Parser.Combinators
import           Hasura.GraphQL.Parser.Monad
import           Hasura.GraphQL.Parser.Schema
