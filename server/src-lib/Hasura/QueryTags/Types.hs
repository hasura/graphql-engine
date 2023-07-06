module Hasura.QueryTags.Types
  ( QueryTagsConfig (..),
    QueryTagsFormat (..),
    defaultQueryTagsConfig,
  )
where

import Autodocodec (HasCodec (codec), named, optionalFieldWithDefault', stringConstCodec)
import Autodocodec qualified as AC
import Data.Aeson
import Data.Aeson.Casing qualified as J
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as T
import Hasura.Prelude

data QueryTagsFormat
  = Standard
  | SQLCommenter
  deriving (Show, Eq, Generic)

instance Hashable QueryTagsFormat

instance NFData QueryTagsFormat

instance ToJSON QueryTagsFormat where
  toJSON = \case
    Standard -> "standard"
    SQLCommenter -> "sqlcommenter"

instance FromJSON QueryTagsFormat where
  parseJSON = withText "QueryTagsFormat"
    $ \t -> case T.toLower t of
      "standard" -> pure Standard
      "sqlcommenter" -> pure SQLCommenter
      _ -> fail errMsg
    where
      errMsg = "Not a valid query tags format value. Use either standard or sqlcommenter"

-- TODO: Replace JSON instances with versions derived from this codec. We'll
-- need to support case-insenitivity on input to make that change without
-- affecting the API.
instance HasCodec QueryTagsFormat where
  codec =
    named "QueryTagsFormat"
      $ stringConstCodec
      $ NonEmpty.fromList
      $ [ (Standard, "standard"),
          (SQLCommenter, "sqlcommenter")
        ]

-- | QueryTagsConfig is the configuration created by the users to control query tags
--
-- This config let's hasura know about the following:
--    1. In what format should the query tags be created
--    2. Should they be appended to the SQL
--    3. Should the request id be part of the query tags (which varies on each request
--       and will cause prepared statements to be re-prepared every time)
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
    _qtcFormat :: !QueryTagsFormat,
    _qtcOmitRequestId :: !Bool
  }
  deriving (Show, Eq, Generic)

instance Hashable QueryTagsConfig

instance NFData QueryTagsConfig

instance ToJSON QueryTagsConfig where
  toJSON = genericToJSON (J.aesonDrop 4 J.snakeCase)
  toEncoding = genericToEncoding (J.aesonDrop 4 J.snakeCase)

instance FromJSON QueryTagsConfig where
  parseJSON = withObject "QueryTagsConfig" $ \o ->
    QueryTagsConfig
      <$> o
      .:? "disabled"
      .!= False
      <*> o
      .:? "format"
      .!= Standard
      <*> o
      .:? "omit_request_id"
      .!= True

instance HasCodec QueryTagsConfig where
  codec =
    AC.object "QueryTagsConfig"
      $ QueryTagsConfig
      <$> optionalFieldWithDefault' "disabled" False
      .== _qtcDisabled
        <*> optionalFieldWithDefault' "format" Standard
      .== _qtcFormat
        <*> optionalFieldWithDefault' "omit_request_id" True
      .== _qtcOmitRequestId
    where
      (.==) = (AC..=)

defaultQueryTagsConfig :: QueryTagsConfig
defaultQueryTagsConfig = QueryTagsConfig False Standard True
