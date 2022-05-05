module Hasura.Backends.DataConnector.API.V0Spec (spec) where

import Hasura.Backends.DataConnector.API.V0.ColumnSpec qualified as ColumnSpec
import Hasura.Backends.DataConnector.API.V0.ConfigSchemaSpec qualified as ConfigSchemaSpec
import Hasura.Backends.DataConnector.API.V0.ExpressionSpec qualified as ExpressionSpec
import Hasura.Backends.DataConnector.API.V0.OrderBySpec qualified as OrderBySpec
import Hasura.Backends.DataConnector.API.V0.QuerySpec qualified as QuerySpec
import Hasura.Backends.DataConnector.API.V0.Scalar.TypeSpec qualified as TypeSpec
import Hasura.Backends.DataConnector.API.V0.Scalar.ValueSpec qualified as ValueSpec
import Hasura.Backends.DataConnector.API.V0.SchemaSpec qualified as SchemaSpec
import Hasura.Backends.DataConnector.API.V0.TableSpec qualified as TableSpec
import Test.Hspec

spec :: Spec
spec = do
  describe "Column" ColumnSpec.spec
  describe "ConfigSchema" ConfigSchemaSpec.spec
  describe "Expression" ExpressionSpec.spec
  describe "OrderBy" OrderBySpec.spec
  describe "Query" QuerySpec.spec
  describe "Scalar.Type" TypeSpec.spec
  describe "Scalar.Value" ValueSpec.spec
  describe "Schema" SchemaSpec.spec
  describe "Table" TableSpec.spec
