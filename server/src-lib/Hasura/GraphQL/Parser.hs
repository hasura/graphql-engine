-- | This module exports the public API to our internal GraphQL query parser
-- combinator language. For more details, see the documentation for 'Parser'.
module Hasura.GraphQL.Parser
  ( Parser
  , parserType
  , runParser
  , bind
  , bindFields

  , boolean
  , int
  , float
  , string
  , identifier
  , uuid
  , json
  , jsonb
  , nonNegativeInt
  , bigInt
  , unsafeRawScalar
  , jsonScalar

  , enum
  , nullable
  , list
  , object
  , selectionSet
  , safeSelectionSet
  , selectionSetObject

  , InputFieldsParser
  , field
  , fieldWithDefault
  , fieldOptional

  , FieldParser
  , ParsedSelection(..)
  , handleTypename
  , selection
  , rawSelection
  , selection_
  , subselection
  , rawSubselection
  , subselection_

  , jsonToGraphQL
  , valueToJSON

  , module Hasura.GraphQL.Parser.Class
  , module Hasura.GraphQL.Parser.Column
  , module Hasura.GraphQL.Parser.Monad
  , module Hasura.GraphQL.Parser.Schema
  ) where

import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Column
import           Hasura.GraphQL.Parser.Internal.Convert
import           Hasura.GraphQL.Parser.Internal.Parser
import           Hasura.GraphQL.Parser.Internal.Scalars
import           Hasura.GraphQL.Parser.Monad
import           Hasura.GraphQL.Parser.Schema
