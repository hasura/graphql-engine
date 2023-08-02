-- | This module exports the public API to our internal GraphQL query parser
-- combinator language. For more details, see the documentation for 'Parser'.
module Hasura.GraphQL.Parser
  ( Directive (..),
    InputFieldsParser (..),
    FieldParser (..),
    ParsedSelection (..),
    Parser (..),
    parserType,
    runParser,
    bind,
    bindField,
    bindFields,
    namedBoolean,
    namedInt,
    namedFloat,
    namedString,
    namedIdentifier,
    boolean,
    int,
    float,
    string,
    identifier,
    uuid,
    json,
    jsonb,
    nonNegativeInt,
    bigInt,
    scientific,
    jsonScalar,
    enum,
    nullable,
    nullableParser,
    nonNullableParser,
    multiple,
    setParserOrigin,
    setFieldParserOrigin,
    setInputFieldsParserOrigin,
    list,
    object,
    selectionSet,
    safeSelectionSet,
    selectionSetInterface,
    selectionSetObject,
    selectionSetUnion,
    field,
    fieldWithDefault,
    fieldWithDefault',
    fieldOptional,
    fieldOptional',
    wrapFieldParser,
    handleTypename,
    selection,
    rawSelection,
    selection_,
    subselection,
    rawSubselection,
    subselection_,
    jsonToGraphQL,
    valueToJSON,
    module Hasura.GraphQL.Parser.Class,
    module Hasura.GraphQL.Parser.ErrorCode,
    module Hasura.GraphQL.Parser.Monad,
    module Hasura.GraphQL.Parser.Names,
    module Hasura.GraphQL.Parser.Schema,
    module Hasura.GraphQL.Parser.Variable,
  )
where

import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.Directives
import Hasura.GraphQL.Parser.ErrorCode
import Hasura.GraphQL.Parser.Internal.Convert
import Hasura.GraphQL.Parser.Internal.Parser
import Hasura.GraphQL.Parser.Internal.Scalars
import Hasura.GraphQL.Parser.Monad
import Hasura.GraphQL.Parser.Names
import Hasura.GraphQL.Parser.Schema
import Hasura.GraphQL.Parser.Variable
