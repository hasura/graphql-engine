module Hasura.RQL.Types.Headers
  ( HeaderConf (..),
    HeaderValue (HVEnv, HVValue),
  )
where

import Autodocodec (HasCodec (codec), bimapCodec, disjointEitherCodec, requiredField')
import Autodocodec qualified as AC
import Data.Aeson
import Data.Text qualified as T
import Data.URL.Template
import Hasura.Base.Instances ()
import Hasura.Prelude

data HeaderConf = HeaderConf HeaderName HeaderValue
  deriving (Show, Eq, Generic)

instance NFData HeaderConf

instance Hashable HeaderConf

type HeaderName = Text

data HeaderValue = HVValue Template | HVEnv Text
  deriving (Show, Eq, Generic)

instance NFData HeaderValue

instance Hashable HeaderValue

instance HasCodec HeaderConf where
  codec = bimapCodec dec enc $ disjointEitherCodec valCodec fromEnvCodec
    where
      valCodec =
        AC.object "HeaderConfValue"
          $ (,)
          <$> requiredField' "name"
          AC..= fst
            <*> requiredField' "value"
          AC..= snd

      fromEnvCodec =
        AC.object "HeaderConfFromEnv"
          $ (,)
          <$> requiredField' "name"
          AC..= fst
            <*> requiredField' "value_from_env"
          AC..= snd

      dec (Left (name, value)) = Right $ HeaderConf name (HVValue value)
      dec (Right (name, valueFromEnv)) =
        if T.isPrefixOf "HASURA_GRAPHQL_" valueFromEnv
          then Left $ "env variables starting with \"HASURA_GRAPHQL_\" are not allowed in value_from_env: " <> T.unpack valueFromEnv
          else Right $ HeaderConf name (HVEnv valueFromEnv)

      enc (HeaderConf name (HVValue val)) = Left (name, val)
      enc (HeaderConf name (HVEnv val)) = Right (name, val)

instance FromJSON HeaderConf where
  parseJSON (Object o) = do
    name <- o .: "name"
    value <- o .:? "value"
    valueFromEnv <- o .:? "value_from_env"
    case (value, valueFromEnv) of
      (Nothing, Nothing) -> fail "expecting value or value_from_env keys"
      (Just val, Nothing) -> do
        template <- parseJSON val
        return $ HeaderConf name (HVValue template)
      (Nothing, Just val) -> do
        when (T.isPrefixOf "HASURA_GRAPHQL_" val)
          $ fail
          $ "env variables starting with \"HASURA_GRAPHQL_\" are not allowed in value_from_env: "
          <> T.unpack val
        return $ HeaderConf name (HVEnv val)
      (Just _, Just _) -> fail "expecting only one of value or value_from_env keys"
  parseJSON _ = fail "expecting object for headers"

instance ToJSON HeaderConf where
  toJSON (HeaderConf name (HVValue val)) = object ["name" .= name, "value" .= toJSON val]
  toJSON (HeaderConf name (HVEnv val)) = object ["name" .= name, "value_from_env" .= val]
