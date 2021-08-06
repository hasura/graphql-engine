-- |

module Hasura.RQL.Types.Network where

import           Hasura.Prelude

import qualified Data.Text                 as T

import           Data.Aeson                as A
import           Test.QuickCheck.Arbitrary as Q


data Network
  = Network
  { _nwHttp  :: !(Maybe Http)
  } deriving (Show, Eq, Generic)

networkTlsAllowlist :: Network -> [TlsAllow]
networkTlsAllowlist (Network (Just (Http l))) = l
networkTlsAllowlist _                         = []

instance Q.Arbitrary Network where
  -- TODO: Decide if this should be extended to actual arbitrary networks
  -- This could prove complicated for testing purposes since the implications
  -- Are difficult to test.
  arbitrary = pure (Network Nothing)

instance FromJSON Network where
  parseJSON = withObject "Network" $ \o -> Network
    <$> o .:? "http"

instance ToJSON Network where
  toJSON (Network h) = object ["http" A..= h]

data Http
  = Http
  { _hTlsAllowList  :: ![TlsAllow]
  } deriving (Show, Eq, Generic)

instance FromJSON Http where
  parseJSON = withObject "HTTP" $ \o -> Http
    <$> o .:? "tls_allowlist" .!= []

instance ToJSON Http where
  toJSON (Http t) = object ["tls_allowlist" A..= t]

data TlsAllow
  = TlsAllow
  { taHost   :: !String
  , taSuffix :: !(Maybe String)
  , taPermit :: !(Maybe [TlsPermission])
  } deriving (Show, Eq, Generic)

instance FromJSON TlsAllow where
  parseJSON j = aString j <|> anObject j
    where
      aString  = withText "TlsAllow"   $ \s -> pure $ TlsAllow (T.unpack s) Nothing Nothing
      anObject = withObject "TlsAllow" $ \o -> TlsAllow
        <$> o .: "host"
        <*> o .:? "suffix"
        <*> o .:? "permissions"

instance ToJSON TlsAllow where
  toJSON (TlsAllow h p a) = object
    [ "host"        A..= h
    , "suffix"      A..= p
    , "permissions" A..= a
    ]

data TlsPermission
  = SelfSigned
  deriving (Show, Eq, Generic, Enum, Bounded)

instance FromJSON TlsPermission where
  parseJSON (String "self-signed") = pure SelfSigned
  parseJSON _ = fail $ "TlsPermission expecting one of " <> intercalate ", " (map (show :: TlsPermission -> String) [minBound .. maxBound])

instance ToJSON TlsPermission where
  toJSON SelfSigned = String "self-signed"

