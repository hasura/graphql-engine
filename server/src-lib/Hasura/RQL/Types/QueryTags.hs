module Hasura.RQL.Types.QueryTags where

import           Hasura.Prelude

import           Data.Aeson
import qualified Data.Aeson.Casing  as J
import qualified Data.Aeson.TH      as J
import qualified Data.Text          as T
import           Hasura.Incremental (Cacheable (..))

data QueryTagsFormat
  = Standard
  | SQLCommenter
  deriving (Show, Eq, Generic)
instance Cacheable QueryTagsFormat
instance Hashable  QueryTagsFormat
instance NFData    QueryTagsFormat


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

data QueryTagsConfig
  = QueryTagsConfig
  { _qtcDisabled :: !Bool
  , _qtcFormat   :: !QueryTagsFormat
  } deriving (Show, Eq, Generic)
instance Cacheable QueryTagsConfig
instance Hashable QueryTagsConfig
instance NFData QueryTagsConfig
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''QueryTagsConfig)

instance FromJSON QueryTagsConfig where
  parseJSON = withObject "QueryTagsConfig" $ \o ->
    QueryTagsConfig
      <$> o .:? "disabled" .!= False
      <*> o .:? "format" .!= Standard

defaultQueryTagsConfig :: QueryTagsConfig
defaultQueryTagsConfig = QueryTagsConfig False Standard
