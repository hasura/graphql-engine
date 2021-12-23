module Hasura.RQL.DDL.Network
  ( checkForHostnameInAllowlistObject,
    dropHostFromAllowList,
    runAddHostToTLSAllowlist,
    runDropHostFromTLSAllowlist,
  )
where

import Data.Text (pack)
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Metadata.Class ()
import Hasura.Prelude
import Hasura.RQL.Types
import Hasura.RQL.Types.Network ()

runAddHostToTLSAllowlist ::
  (QErrM m, CacheRWM m, MetadataM m) =>
  TlsAllow ->
  m EncJSON
runAddHostToTLSAllowlist tlsAllowListEntry@TlsAllow {..} = do
  networkMetadata <- _metaNetwork <$> getMetadata

  when (null taHost) $ do
    throw400 BadRequest $ "key \"host\" cannot be empty"

  when (checkForHostInTLSAllowlist taHost (tlsList networkMetadata)) $ do
    throw400 AlreadyExists $
      "the host " <> dquote (pack taHost) <> " already exists in the allowlist"

  withNewInconsistentObjsCheck $
    buildSchemaCacheFor (MOHostTlsAllowlist taHost) $
      addHostToTLSAllowList tlsAllowListEntry

  pure successMsg
  where
    tlsList nm = networkTlsAllowlist nm

runDropHostFromTLSAllowlist ::
  (QErrM m, CacheRWM m, MetadataM m) =>
  DropHostFromTLSAllowlist ->
  m EncJSON
runDropHostFromTLSAllowlist (DropHostFromTLSAllowlist hostname) = do
  networkMetadata <- _metaNetwork <$> getMetadata

  when (null hostname) $ do
    throw400 BadRequest $ "hostname cannot be empty"

  unless (checkForHostInTLSAllowlist hostname (networkTlsAllowlist networkMetadata)) $ do
    throw400 NotExists $
      "the host " <> dquote (pack hostname) <> " isn't present in the allowlist"

  withNewInconsistentObjsCheck $
    buildSchemaCache $
      dropHostFromAllowList hostname

  pure successMsg

addHostToTLSAllowList :: TlsAllow -> MetadataModifier
addHostToTLSAllowList tlsaObj = MetadataModifier $ \m ->
  m {_metaNetwork = Network $ (tlsList m) ++ [tlsaObj]}
  where
    tlsList md = networkTlsAllowlist (_metaNetwork md)

dropHostFromAllowList :: String -> MetadataModifier
dropHostFromAllowList host = MetadataModifier $ \m ->
  m {_metaNetwork = Network $ filteredList m}
  where
    tlsList md = networkTlsAllowlist (_metaNetwork md)

    filteredList md = filter (not . checkForHostnameInAllowlistObject host) (tlsList md)

checkForHostnameInAllowlistObject :: String -> TlsAllow -> Bool
checkForHostnameInAllowlistObject host tlsa = host == (taHost tlsa)

checkForHostInTLSAllowlist :: String -> [TlsAllow] -> Bool
checkForHostInTLSAllowlist host tlsAllowList =
  any (checkForHostnameInAllowlistObject host) tlsAllowList
