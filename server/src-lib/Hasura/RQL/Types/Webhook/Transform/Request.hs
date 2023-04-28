{-# LANGUAGE DeriveAnyClass #-}

module Hasura.RQL.Types.Webhook.Transform.Request
  ( RequestTransformCtx (..),
    Version (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, (.=))
import Data.Aeson qualified as J
import Hasura.Prelude
import Hasura.RQL.Types.Webhook.Transform.Class (TemplatingEngine (..))
import Hasura.Session (SessionVariables)

-- | Common context that is made available to all request transformations.
data RequestTransformCtx = RequestTransformCtx
  { rtcBaseUrl :: Maybe J.Value,
    rtcBody :: J.Value,
    rtcSessionVariables :: Maybe SessionVariables,
    rtcQueryParams :: Maybe J.Value,
    rtcEngine :: TemplatingEngine
  }

instance ToJSON RequestTransformCtx where
  toJSON RequestTransformCtx {..} =
    let required =
          [ "body" .= rtcBody,
            "session_variables" .= rtcSessionVariables
          ]
        optional =
          [ ("base_url" .=) <$> rtcBaseUrl,
            ("query_params" .=) <$> rtcQueryParams
          ]
     in J.object (required <> catMaybes optional)

-------------------------------------------------------------------------------

-- | 'RequestTransform' Versioning
data Version
  = V1
  | V2
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable, NFData)

instance FromJSON Version where
  parseJSON v = do
    version :: Int <- J.parseJSON v
    case version of
      1 -> pure V1
      2 -> pure V2
      i -> fail $ "expected 1 or 2, encountered " ++ show i

instance ToJSON Version where
  toJSON = \case
    V1 -> J.toJSON @Int 1
    V2 -> J.toJSON @Int 2
