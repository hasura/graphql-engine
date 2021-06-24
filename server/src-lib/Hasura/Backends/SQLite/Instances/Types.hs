{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.SQLite.Instances.Types where

import           Hasura.Prelude

import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.Backends.SQLite.Types
import           Hasura.Base.Error
import           Hasura.Base.Instances         ()
import           Hasura.RQL.DDL.Headers        ()
import           Hasura.RQL.Types.Backend
import           Hasura.SQL.Backend


instance Backend 'SQLite where

  -- Required
  type SourceConfig            'SQLite = SLSourceConfig
  type SourceConnConfiguration 'SQLite = SLFilePath

  snakeCaseTableName = id
  functionToTable = id
  tableToFunction = id
  tableGraphQLName t = G.mkName t
    `onNothing` throw400 ValidationFailed "unsupported table name"

  -- Optional / Advanced
  type Identifier              'SQLite = Void
  type Alias                   'SQLite = Text
  type TableName               'SQLite = Text
  type FunctionName            'SQLite = Text
  type FunctionArgType         'SQLite = Void
  type ConstraintName          'SQLite = Void
  type BasicOrderType          'SQLite = Void
  type NullsOrderType          'SQLite = Void
  type CountType               'SQLite = Void
  type Column                  'SQLite = Text
  type ScalarValue             'SQLite = Text
  type ScalarType              'SQLite = Text
  type BooleanOperators        'SQLite = Const Void
  type SQLExpression           'SQLite = Text
  type SQLOperator             'SQLite = Text

  type ExtraTableMetadata      'SQLite = ()

  type XComputedField          'SQLite = XDisable
  type XRelay                  'SQLite = XDisable
  type XNodesAgg               'SQLite = XDisable

  functionArgScalarType = undefined
  isComparableType = undefined
  isNumType = undefined
  textToScalarValue = undefined
  parseScalarValue = undefined
  scalarValueToJSON = undefined
  functionGraphQLName = undefined
  scalarTypeGraphQLName = undefined
