{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.Types.QueryTags
  ( QueryTagsConfig (..),
    QueryTagsFormat (..),
    defaultQueryTagsConfig,
  )
where

import Data.Aeson
import Data.Aeson.Casing qualified as J
import Data.Aeson.TH qualified as J
import Data.Text qualified as T
import Hasura.Incremental (Cacheable (..))
import Hasura.Prelude

data QueryTagsFormat
  = Standard
  | SQLCommenter
  deriving (Show, Eq, Generic)

instance Cacheable QueryTagsFormat

instance Hashable QueryTagsFormat

instance NFData QueryTagsFormat

instance ToJSON QueryTagsFormat where
  toJSON = \case
    Standard -> "standard"
    SQLCommenter -> "sqlcommenter"

instance FromJSON QueryTagsFormat where
  parseJSON = withText "QueryTagsFormat" $
    \t -> case T.toLower t of
      "standard" -> pure Standard
      "sqlcommenter" -> pure SQLCommenter
      _ -> fail errMsg
    where
      errMsg = "Not a valid query tags format value. Use either standard or sqlcommenter"

-- | QueryTagsConfig is the configuration created by the users to control query tags
--
-- This config let's hasura know about the followingL
--    1. In what format should the query tags be created
--    2. Should they be appended to the SQL
--
-- FWIW, `QueryTagsConfig` are coupled along with the Source metadata. So you can
-- also think `QueryTagsConfig` as the query tags configuration for each source.
--
-- The workflow is something like this:
--
--    1. The `QueryTagsConfig` for a source is created from the metadata we get from
--       the user.
--    2. This configuration is packaged (for the lack of better word) along with
--       SourceConfigWith
--    3. These query tags configuration are extracted from the `SourceConfigWith` in
--       the `mkDBQueryPlan`, `mkDBMutationPlan`, `mkDBSubscriptionPlan` functions
--       and are passed along to the point where the actual SQL generation takes
--       place
--
-- Note that, it is important for `QueryTagsConfig` to be a part of
-- `SourceConfigWith` because that's the only sane way (that we can think of) the
-- `mkDB..Plan` functions can get the QueryTagsConfig.
data QueryTagsConfig = QueryTagsConfig
  { _qtcDisabled :: !Bool,
    _qtcFormat :: !QueryTagsFormat
  }
  deriving (Show, Eq, Generic)

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
