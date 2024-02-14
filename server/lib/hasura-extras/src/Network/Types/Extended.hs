module Network.Types.Extended
  ( AddHostToTLSAllowlist,
    DropHostFromTLSAllowlist (..),
    Network (..),
    TlsAllow (..),
    TlsPermission (..),
    emptyNetwork,
  )
where

import Autodocodec (HasCodec, optionalField', optionalFieldWithDefault', requiredField')
import Autodocodec qualified as AC
import Autodocodec.Extended (boundedEnumCodec)
import Data.Aeson as J
import Data.Text qualified as T
import Hasura.Prelude

data Network = Network
  { networkTlsAllowlist :: [TlsAllow]
  }
  deriving (Show, Eq, Generic)

instance HasCodec Network where
  codec =
    AC.object "Network"
      $ Network
      <$> optionalFieldWithDefault' "tls_allowlist" []
      AC..= networkTlsAllowlist

instance FromJSON Network where
  parseJSON = withObject "Network" $ \o -> Network <$> o .:? "tls_allowlist" .!= []

instance ToJSON Network where
  toJSON (Network t) = object ["tls_allowlist" J..= t]

emptyNetwork :: Network
emptyNetwork = Network []

data TlsAllow = TlsAllow
  { taHost :: String,
    taSuffix :: Maybe String,
    taPermit :: Maybe [TlsPermission]
  }
  deriving (Show, Read, Eq, Generic)

instance HasCodec TlsAllow where
  codec =
    AC.object "TlsAllow"
      $ TlsAllow
      <$> requiredField' "host"
      AC..= taHost
        <*> optionalField' "suffix"
      AC..= taSuffix
        <*> optionalField' "permissions"
      AC..= taPermit

instance FromJSON TlsAllow where
  parseJSON j = aString j <|> anObject j
    where
      -- TODO: investigate if `withText` parser is needed anymore
      aString = withText "TlsAllow" $ \s ->
        if T.null s
          then fail "missing \"host\" field in input"
          else pure $ TlsAllow (T.unpack s) Nothing Nothing

      anObject = withObject "TlsAllow" $ \o ->
        TlsAllow
          <$> o
          .: "host"
          <*> o
          .:? "suffix"
          <*> o
          .:? "permissions"

instance ToJSON TlsAllow where
  toJSON (TlsAllow h p a) =
    object
      [ "host" J..= h,
        "suffix" J..= p,
        "permissions" J..= a
      ]

data TlsPermission
  = SelfSigned
  deriving (Show, Read, Eq, Generic, Enum, Bounded)

instance HasCodec TlsPermission where
  codec = boundedEnumCodec \case
    SelfSigned -> "self-signed"

instance FromJSON TlsPermission where
  parseJSON (String "self-signed") = pure SelfSigned
  parseJSON _ =
    fail
      $ "TlsPermission expecting one of "
      <> intercalate ", " (map (show :: TlsPermission -> String) [minBound .. maxBound])

instance ToJSON TlsPermission where
  toJSON SelfSigned = String "self-signed"

type AddHostToTLSAllowlist = TlsAllow

data DropHostFromTLSAllowlist = DropHostFromTLSAllowlist
  { dhftaHost :: String,
    dhftaSuffix :: Maybe String
  }
  deriving (Show, Eq)

instance FromJSON DropHostFromTLSAllowlist where
  parseJSON = withObject "DropHostFromTLSAllowlist" $ \o ->
    DropHostFromTLSAllowlist
      <$> o
      .: "host"
      <*> o
      .:? "suffix"

instance ToJSON DropHostFromTLSAllowlist where
  toJSON (DropHostFromTLSAllowlist h p) =
    object
      [ "host" J..= h,
        "suffix" J..= p
      ]
