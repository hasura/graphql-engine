module Hasura.RQL.Types.BoolExp
  ( BoolExpRHSParser (..),
    BoolExpResolver (..),
  )
where

import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Table
import Hasura.SQL.Backend

-- | Context to parse a RHS value in a boolean expression
data BoolExpRHSParser (b :: BackendType) m v = BoolExpRHSParser
  { -- | Parse a JSON value with enforcing a column type
    _berpValueParser :: ValueParser b m v,
    -- | Required for a computed field SQL function with session argument
    _berpSessionValue :: v
  }

-- | A function which resolves boolean expression from given table fields
newtype BoolExpResolver b m v = BoolExpResolver
  { getBoolExpResolver ::
      BoolExpRHSParser b m v ->
      TableName b ->
      FieldInfoMap (FieldInfo b) ->
      GBoolExp b ColExp ->
      m (AnnBoolExp b v)
  }
