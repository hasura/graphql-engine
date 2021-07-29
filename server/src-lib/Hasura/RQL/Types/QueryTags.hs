module Hasura.RQL.Types.QueryTags where

import           Hasura.Prelude

import           Control.Lens
import           Data.Aeson
import qualified Data.Aeson.Casing   as J
import qualified Data.Aeson.TH       as J
import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as T


data QueryTagsFormat
  = Standard
  | SQLCommenter
  deriving (Show, Eq, Generic)

instance ToJSON QueryTagsFormat where
  toJSON = \case
    Standard     -> "standard"
    SQLCommenter -> "sqlcommenter"

instance FromJSON QueryTagsFormat where
  parseJSON = withText "QueryTagsFormat" $
    \t -> case T.toLower t of
      "standard"     -> pure Standard
      "sqlcommenter" -> pure SQLCommenter
      _              -> fail errMsg
    where
      errMsg = "Not a valid query tags format value. Use either standard or sqlcommenter"

-- Query Tags configuration for each database source
data QueryTagsPerSourceConfig
  = QueryTagsPerSourceConfig
  { _qtpscFormat   :: !(Maybe QueryTagsFormat)
  , _qtpscDisabled :: !Bool
  } deriving (Show, Eq, Generic)
$(J.deriveToJSON (J.aesonDrop 6 J.snakeCase){J.omitNothingFields = True} ''QueryTagsPerSourceConfig)

instance FromJSON QueryTagsPerSourceConfig where
  parseJSON = withObject "QueryTagsPerSourceConfig" $ \o ->
    QueryTagsPerSourceConfig
      <$> o .:? "format"
      <*> o .:? "disabled" .!= False

-- Query Tag Configuration that is set in metadata
data QueryTagsConfig
  = QueryTagsConfig
  { _qtmcDefaultFormat          :: !(QueryTagsFormat) -- If no format is set then it will be standard
  , _qtmcPerSourceConfiguration :: !((Map.HashMap Text QueryTagsPerSourceConfig))
  } deriving (Show, Eq, Generic)
$(J.deriveToJSON (J.aesonDrop 5 J.snakeCase) ''QueryTagsConfig)

instance FromJSON QueryTagsConfig  where
  parseJSON = withObject "QueryTagsConfig" $ \o ->
    QueryTagsConfig
      <$> o .:? "default_format" .!= (Standard)
      <*> o .:? "per_source_configuration" .!= mempty

emptyQueryTagsConfig :: QueryTagsConfig
emptyQueryTagsConfig = QueryTagsConfig (Standard) mempty

$(makeLenses ''QueryTagsPerSourceConfig)
$(makeLenses ''QueryTagsConfig)
$(makeLenses ''QueryTagsFormat)

-- | Used to store the Query tags configuration for each source after processing the
-- 'QueryTagsConfig'
data QueryTagsSourceConfig
  = QueryTagsSourceConfig
  { _qtscFormat   :: !QueryTagsFormat
  , _qtscDisabled :: !Bool
  } deriving (Show, Eq, Generic)


