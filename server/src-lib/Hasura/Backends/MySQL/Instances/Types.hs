{-# OPTIONS_GHC -fno-warn-orphans #-}


module Hasura.Backends.MySQL.Instances.Types where


import qualified Data.Aeson                    as J
import qualified Database.MySQL.Base.Types     as MySQL
import qualified Hasura.Backends.MySQL.Types   as MySQL
import           Hasura.Base.Error
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers        ()
import           Hasura.RQL.Types.Backend
import           Hasura.SQL.Backend
import qualified Language.GraphQL.Draft.Syntax as G


instance Arbitrary Void where
  arbitrary = error "MySQL backend does not support this operation yet."


instance Backend 'MySQL where
  type SourceConfig            'MySQL = MySQL.SourceConfig
  type SourceConnConfiguration 'MySQL = MySQL.ConnSourceConfig
  type Identifier              'MySQL = Void
  type TableName               'MySQL = MySQL.TableName
  type RawFunctionInfo         'MySQL = Void -- MySQL.FunctionName
  type FunctionName            'MySQL = Void -- MySQL.FunctionName
  type FunctionArgType         'MySQL = Void
  type ConstraintName          'MySQL = MySQL.ConstraintName
  type BasicOrderType          'MySQL = MySQL.Order
  type NullsOrderType          'MySQL = MySQL.NullsOrder
  type CountType               'MySQL = Void -- MySQL.Countable MySQL.ColumnName
  type Column                  'MySQL = MySQL.Column
  type ScalarValue             'MySQL = MySQL.ScalarValue
  type ScalarType              'MySQL = MySQL.ScalarType -- DB.Type
  type SQLExpression           'MySQL = MySQL.Expression
  type SQLOperator             'MySQL = Void -- MySQL.Op
  type BooleanOperators        'MySQL = Const Void
  type XComputedField          'MySQL = Void
  type XRelay                  'MySQL = Void
  type XNodesAgg               'MySQL = XEnable
  type ExtraTableMetadata      'MySQL = ()

  functionArgScalarType :: FunctionArgType 'MySQL -> ScalarType 'MySQL
  functionArgScalarType = error "functionArgScalarType: not implemented yet"

  isComparableType :: ScalarType 'MySQL -> Bool
  isComparableType = isNumType @'MySQL -- TODO: For now we only allow comparisons for numeric types

  isNumType :: ScalarType 'MySQL -> Bool
  isNumType = \case
    MySQL.Decimal    -> True
    MySQL.Tiny       -> True
    MySQL.Short      -> True
    MySQL.Long       -> True
    MySQL.Float      -> True
    MySQL.Double     -> True
    MySQL.Null       -> False
    MySQL.Timestamp  -> False
    MySQL.LongLong   -> True
    MySQL.Int24      -> True
    MySQL.Date       -> False
    MySQL.Time       -> False
    MySQL.DateTime   -> False
    MySQL.Year       -> False
    MySQL.NewDate    -> False
    MySQL.VarChar    -> False
    MySQL.Bit        -> False
    MySQL.NewDecimal -> True
    MySQL.Enum       -> False
    MySQL.Set        -> False
    MySQL.TinyBlob   -> False
    MySQL.MediumBlob -> False
    MySQL.LongBlob   -> False
    MySQL.Blob       -> False
    MySQL.VarString  -> False
    MySQL.String     -> False
    MySQL.Geometry   -> False
    MySQL.Json       -> False

  textToScalarValue :: Maybe Text -> ScalarValue 'MySQL
  textToScalarValue = error "MySQL backend does not support this operation yet."

  parseScalarValue :: ScalarType 'MySQL -> J.Value -> Either QErr (ScalarValue 'MySQL)
  parseScalarValue = error "MySQL backend does not support this operation yet."

  scalarValueToJSON :: ScalarValue 'MySQL -> J.Value
  scalarValueToJSON = error "MySQL backend does not support this operation yet."

  functionToTable :: FunctionName 'MySQL -> TableName 'MySQL
  functionToTable = error "MySQL backend does not support this operation yet."

  tableToFunction :: TableName 'MySQL -> FunctionName 'MySQL
  tableToFunction = error "MySQL backend does not support this operation yet."

  tableGraphQLName :: TableName 'MySQL -> Either QErr G.Name
  tableGraphQLName MySQL.TableName{..} =
    let gName = schema <> "_" <> name
     in (G.mkName gName)
          `onNothing`
            throw400 ValidationFailed ("TableName " <> gName <> " is not a valid GraphQL identifier")

  functionGraphQLName :: FunctionName 'MySQL -> Either QErr G.Name
  functionGraphQLName = error "MySQL backend does not support this operation yet."

  scalarTypeGraphQLName :: ScalarType 'MySQL -> Either QErr G.Name
  scalarTypeGraphQLName = error "MySQL backend does not support this operation yet."

  snakeCaseTableName :: TableName 'MySQL -> Text
  snakeCaseTableName = error "MySQL backend does not support this operation yet."
