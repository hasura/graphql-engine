module Network.HTTP.Client.DynamicTlsPermissions
  ( dynamicTlsSettings,
  )
where

import Control.Exception.Safe (Exception, Typeable, impureThrow)
import Data.ByteString.Char8 qualified as BC
import Data.Default.Class qualified as HTTP
import Data.X509 qualified as HTTP
import Data.X509.CertificateStore qualified as HTTP
import Data.X509.Validation qualified as HTTP
import GHC.Exception (Exception (displayException))
import Hasura.Prelude
import Network.Connection qualified as HTTP
import Network.TLS qualified as HTTP
import Network.TLS.Extra qualified as TLS
import Network.Types.Extended (TlsAllow (TlsAllow), TlsPermission (SelfSigned))
import System.X509 qualified as HTTP

newtype TlsServiceDefinitionError = TlsServiceDefinitionError
  { tlsServiceDefinitionError :: String
  }
  deriving (Show, Typeable)

instance Exception TlsServiceDefinitionError where
  displayException (TlsServiceDefinitionError msg) = "TlsServiceDefinitionError: " <> show msg

errorE :: String -> c
errorE = impureThrow . TlsServiceDefinitionError

dynamicTlsSettings :: IO [TlsAllow] -> IO HTTP.TLSSettings
dynamicTlsSettings currentAllow = do
  systemStore <- HTTP.getSystemCertificateStore
  return (tlsSettingsComplex systemStore)
  where
    tlsSettingsComplex :: HTTP.CertificateStore -> HTTP.TLSSettings
    tlsSettingsComplex systemStore = HTTP.TLSSettings (clientParams systemStore)

    clientParams :: HTTP.CertificateStore -> HTTP.ClientParams
    clientParams systemStore =
      (HTTP.defaultParamsClient hostName serviceIdBlob)
        { HTTP.clientSupported = HTTP.def {HTTP.supportedCiphers = TLS.ciphersuite_default}, -- supportedCiphers :: [Cipher]	Supported cipher methods. The default is empty, specify a suitable cipher list. ciphersuite_default is often a good choice.  Default: [] -- https://hackage.haskell.org/package/tls-1.5.5/docs/Network-TLS.html#t:Cipher
          HTTP.clientShared = HTTP.def {HTTP.sharedCAStore = systemStore},
          HTTP.clientHooks =
            HTTP.def
              { HTTP.onServerCertificate = certValidation
              }
        }

    certValidation :: HTTP.CertificateStore -> HTTP.ValidationCache -> HTTP.ServiceID -> HTTP.CertificateChain -> IO [HTTP.FailedReason]
    certValidation certStore validationCache sid chain = do
      res <- HTTP.onServerCertificate HTTP.def certStore validationCache sid chain
      allowList <- currentAllow
      if any (allowed sid res) allowList
        then pure []
        else pure res

    -- These always seem to be overwritten when a connection is established
    -- Should leave as errors in this case in order to validate this assumption.
    -- TODO: Is there any way to define this in terms of a pure exception?
    hostName = errorE "hostname in HTTP client defaultParamsClient accessed - this should never happen"
    serviceIdBlob = errorE "serviceIdBlob in HTTP client defaultParamsClient accessed - this should never happen"

    -- Checks that:

    allowed :: (String, BC.ByteString) -> [HTTP.FailedReason] -> TlsAllow -> Bool
    allowed (sHost, sPort) res (TlsAllow aHost aPort aPermit) =
      (sHost == aHost)
        && (BC.unpack sPort ==? aPort)
        && all (\x -> any (($ x) . permitted) (fromMaybe [SelfSigned] aPermit)) res
    -- TODO: Could clean up this check some more.

    -- Comments on failure reasons taken from https://hackage.haskell.org/package/x509-validation-1.4.7/docs/src/Data-X509-Validation.html
    -- The permitted function takes high-level concerns and translates then into certain permitted errors

    permitted SelfSigned HTTP.SelfSigned = True -- Certificate is self signed
    permitted SelfSigned (HTTP.NameMismatch _) = True -- Connection name and certificate do not match
    permitted SelfSigned HTTP.LeafNotV3 = True -- Only authorized an X509.V3 certificate as leaf certificate.
    permitted SelfSigned _ = False

    _ ==? Nothing = True
    a ==? Just a' = a == a'
