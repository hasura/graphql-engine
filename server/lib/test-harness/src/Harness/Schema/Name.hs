-- | This module defines `SchemaName`, for naming DB schemas/datasets used in
-- tests
module Harness.Schema.Name
  ( SchemaName (..),
    resolveReferenceSchema,
  )
where

import Data.Aeson (ToJSON (..))
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml
import Safe (lastMay)
import Prelude

newtype SchemaName = SchemaName {unSchemaName :: Text}
  deriving newtype (Eq, Show, Semigroup)

instance ToJSON SchemaName where
  toJSON (SchemaName sn) = toJSON sn

instance IsString SchemaName where
  fromString s = SchemaName (T.pack s)

instance ToGraphqlString SchemaName where
  showGql (SchemaName sn) = T.unpack sn

instance ToYamlString SchemaName where
  showYml (SchemaName sn) = T.unpack sn

-- | when given a list of qualifiers, we assume that the schema is the last one
-- io Postgres, it'll be the only item
-- in BigQuery, it could be ['project','schema']
resolveReferenceSchema :: [Text] -> Maybe SchemaName
resolveReferenceSchema qualifiers =
  case lastMay qualifiers of
    Nothing -> Nothing
    Just schemaName -> Just (SchemaName schemaName)
