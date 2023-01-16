{-# LANGUAGE UndecidableInstances #-}

-- | Types concerned with user-specified custom SQL fragments.
module Hasura.CustomSQL
  ( CustomSQLParameter (..),
  )
where

import Autodocodec (HasCodec (codec))
import Autodocodec qualified as AC
import Data.Aeson
import Hasura.Prelude

----------------------------------

newtype CustomSQLParameterName = CustomSQLParameterName {cspnName :: Text}
  deriving newtype (Show, Eq, FromJSON, ToJSON)

instance HasCodec CustomSQLParameterName where
  codec = AC.dimapCodec CustomSQLParameterName cspnName codec

newtype CustomSQLParameterType = CustomSQLParameterType {cspnType :: Text}
  deriving newtype (Show, Eq, FromJSON, ToJSON)

instance HasCodec CustomSQLParameterType where
  codec = AC.dimapCodec CustomSQLParameterType cspnType codec

data CustomSQLParameter = CustomSQLParameter
  { cspName :: CustomSQLParameterName,
    cspType :: CustomSQLParameterType
  }
  deriving (Show, Eq)

instance FromJSON CustomSQLParameter where
  parseJSON = withObject "CustomSQLParameter" $ \o -> do
    cspName <- o .: "name"
    cspType <- o .: "type"
    pure CustomSQLParameter {..}

instance ToJSON CustomSQLParameter where
  toJSON CustomSQLParameter {..} =
    object
      [ "name" .= cspName,
        "type" .= cspType
      ]

instance HasCodec CustomSQLParameter where
  codec =
    AC.object "CustomSQLParameter" $
      CustomSQLParameter
        <$> AC.requiredField' "name" AC..= cspName
        <*> AC.requiredField' "type" AC..= cspType
