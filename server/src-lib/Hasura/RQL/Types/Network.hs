module Hasura.RQL.Types.Network where

import           Hasura.Prelude

import qualified Data.Text                 as T

import           Data.Aeson                as A
import           Test.QuickCheck.Arbitrary as Q


data Network
  = Network
  { networkTlsAllowlist  :: ![TlsAllow]
  } deriving (Show, Eq, Generic)

instance Q.Arbitrary Network where
  -- TODO: Decide if the arbitrary instance should be extended to actual arbitrary networks
  -- This could prove complicated for testing purposes since the implications
  -- Are difficult to test.
  arbitrary = pure (Network [])

instance FromJSON Network where
  parseJSON = withObject "Network" $ \o -> Network <$> o .:? "tls_allowlist" .!= []

instance ToJSON Network where
  toJSON (Network t) = object ["tls_allowlist" A..= t]

emptyNetwork :: Network
emptyNetwork = Network []

data TlsAllow
  = TlsAllow
  { taHost   :: !String
  , taSuffix :: !(Maybe String)
  , taPermit :: !(Maybe [TlsPermission])
  } deriving (Show, Eq, Generic)

instance FromJSON TlsAllow where
  parseJSON j = aString j <|> anObject j
    where
      aString  = withText "TlsAllow" $ \s ->
        if T.null s
          then fail "missing \"host\" field in input"
          else pure $ TlsAllow (T.unpack s) Nothing Nothing

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
  parseJSON _ = fail $
    "TlsPermission expecting one of " <> intercalate ", " (map (show :: TlsPermission -> String) [minBound .. maxBound])

instance ToJSON TlsPermission where
  toJSON SelfSigned = String "self-signed"

type AddHostToTLSAllowlist = TlsAllow

data DropHostFromTLSAllowlist = DropHostFromTLSAllowlist { _dhftaHost :: !String }
  deriving (Show, Eq)

instance FromJSON DropHostFromTLSAllowlist where
  parseJSON = withObject "DropHostFromTLSAllowlist" $ \o ->
    DropHostFromTLSAllowlist <$> o .: "host"

instance ToJSON DropHostFromTLSAllowlist where
  toJSON (DropHostFromTLSAllowlist h) = object ["host" A..= h]
