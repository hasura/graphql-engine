{-# LANGUAGE TemplateHaskell #-}

-- | Postgres Types CitusExtraTableMetadata
--
-- Additional metadata information for Citus tables.
--
-- See https://www.citusdata.com/blog/2017/07/27/database-table-types-with-citus-and-postgres/
-- for more details on the Citus table types.
module Hasura.Backends.Postgres.Types.CitusExtraTableMetadata
  ( ExtraTableMetadata (..),
  )
where

import Data.Aeson.Casing qualified as JC
import Data.Aeson.TH qualified as J
import Data.Typeable (Typeable)
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

data ExtraTableMetadata
  = Local
  | Reference
  | Distributed {distributionColumn :: Text}
  deriving stock (Show, Eq, Generic, Typeable)

instance Hashable ExtraTableMetadata

instance Cacheable ExtraTableMetadata

instance NFData ExtraTableMetadata

$(J.deriveJSON J.defaultOptions {J.constructorTagModifier = JC.snakeCase, J.fieldLabelModifier = JC.snakeCase} ''ExtraTableMetadata)
