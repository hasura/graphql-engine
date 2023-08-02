-- | Postgres Types CitusExtraTableMetadata
--
-- Additional metadata information for Citus tables.
--
-- See https://www.citusdata.com/blog/2017/07/27/database-table-types-with-citus-and-postgres/
-- for more details on the Citus table types.
module Hasura.Backends.Postgres.Types.CitusExtraTableMetadata
  ( ExtraTableMetadata (Local, Reference, Distributed, tableType),
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as JC
import Data.Typeable (Typeable)
import Hasura.Prelude
import Hasura.RQL.Types.Source.Table (SourceTableType)

data ExtraTableMetadata
  = Local {tableType :: SourceTableType}
  | Reference {tableType :: SourceTableType}
  | Distributed {_distributionColumn :: Text, tableType :: SourceTableType}
  deriving stock (Show, Eq, Generic, Typeable)

instance Hashable ExtraTableMetadata

instance NFData ExtraTableMetadata

instance J.FromJSON ExtraTableMetadata where
  parseJSON =
    J.genericParseJSON
      J.defaultOptions
        { J.constructorTagModifier = JC.snakeCase,
          J.fieldLabelModifier = JC.snakeCase . dropUnderscore
        }

instance J.ToJSON ExtraTableMetadata where
  toJSON =
    J.genericToJSON
      J.defaultOptions
        { J.constructorTagModifier = JC.snakeCase,
          J.fieldLabelModifier = JC.snakeCase . dropUnderscore
        }
  toEncoding =
    J.genericToEncoding
      J.defaultOptions
        { J.constructorTagModifier = JC.snakeCase,
          J.fieldLabelModifier = JC.snakeCase . dropUnderscore
        }

dropUnderscore :: String -> String
dropUnderscore ('_' : rest) = rest
dropUnderscore str = str
